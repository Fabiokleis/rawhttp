%%%-------------------------------------------------------------------
%% @doc rawhttp_server public API
%% @end
%%%-------------------------------------------------------------------

-module(rawhttp_server).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
code_change/3, terminate/2]).

%% tcp socket connection state
-record(state, {socket :: inet:socket(), state :: init | router }).

send(Socket, Str, _Args) ->
    ok = gen_tcp:send(Socket, Str),
    ok = inet:setopts(Socket, [{active, once}]),
    ok.

start_link(Socket) ->
    gen_server:start_link(?MODULE, Socket, []).
 
init(Socket) ->
    %% Because accepting a connection is a blocking function call,
    %% we can not do it in here. Forward to the server loop!
    gen_server:cast(self(), accept), %% call handle_cast(accept...)
    {ok, #state{socket=Socket, state=init}}.

%% We never need you, handle_call!
handle_call(_E, _From, State) ->
    {noreply, State}.

handle_cast(accept, S = #state{socket=ListenSocket}) ->
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket), %% get received tcp socket
    rawhttp_sup:start_socket(), %% a new acceptor is born, praise the lord
    {noreply, S#state{socket=AcceptSocket, state=router}}. 

%%  <<"GET / HTTP/1.1\r\nHost: localhost:8123\r\nUser-Agent: curl/8.0.1\r\nAccept: */*\r\n\r\n">>
%% <<"POST / HTTP/1.1\r\nHost: localhost:8123\r\nUser-Agent: curl/8.0.1\r\nAccept: */*\r\n\r\n">>

raw_not_found() ->
    "HTTP/1.1 404 Not Found
Server: erlang
Content-Type: text/html; charset=utf-8

Not Found.\r\n".

raw_response_ok(Body) ->
    "HTTP/1.1 200 OK
Server: erlang
Content-Type: text/html; charset=utf-8
" ++ Body.

get_file(FileName) ->
    {ok, Bin} = file:read_file(FileName),
    Bin.

rawget("/") ->
    "HTTP/1.1 200 OK
Server: erlang
Content-Type: text/html; charset=utf-8

Hello World!\r\n";

rawget([_S|Object]) -> 
    io:format("cwd: ~p~n", [file:get_cwd()]),
    case filelib:find_file(Object, "views") of
	{ok, FileName} -> raw_response_ok("\r\n" ++ get_file(FileName));
	{error, not_found} -> raw_not_found()
    end.
       
router(<<"GET ", RawHeaders/bitstring>>) -> 
    [Get|Headers] = string:lexemes(bitstring_to_list(RawHeaders), [[$\r, $\n]]),

    [Object|_Http] = string:split(Get, " "),
    io:format("get ~p headers: ~p~n", [Object, Headers]),    

    rawget(Object);

router(<<"POST ", _Headers/bitstring>>) -> 
    "cant post";
router(_) -> "unkown".

handle_info({tcp, Socket, Msg}, S = #state{state=router}) ->
    io:format("http request: ~p~n", [Msg]),
    send(Socket, router(Msg), S), {stop, normal, S};

handle_info({tcp_closed, _Socket}, S) ->
    io:format("~p: closed socket~n", [S]),
    {noreply, S}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(normal, _ = #state{socket=Socket}) ->
    ok = gen_tcp:close(Socket),
    ok;

terminate(Reason, State = #state{socket=Socket}) ->
    ok = gen_tcp:close(Socket),
    io:format("~p: terminate reason: ~p~n", [State, Reason]),
    ok.
