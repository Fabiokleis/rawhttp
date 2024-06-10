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

handle_cast(accept, #state{socket=ListenSocket} = S) ->
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket), %% get received tcp socket
    rawhttp_sup:start_socket(), %% a new acceptor is born, praise the lord
    {noreply, S#state{socket=AcceptSocket, state=router}}. 

header_date({{Year, Month, Day}, {Hour, Mins, Secs}}) ->
    MStr = case Month of
	       1 -> "Jan";
	       2 -> "Feb";
	       3 -> "Mar";
	       4 -> "Apr";
	       5 -> "May";
	       6 -> "Jun";
	       7 -> "Jul";
	       8 -> "Aug";
	       9 -> "Sep";
	       10 -> "Oct";
	       11 -> "Nov";
	       12 -> "Dec"
	   end,

    Wday = case calendar:day_of_the_week(Year, Month, Day) of
	       1 -> "Sun";
	       2 -> "Mon";
	       3 -> "Tue";
	       4 -> "Wed";
	       5 -> "Thu";
	       6 -> "Fri";
	       7 -> "Sat"
    end,
	 
    io_lib:format("~s, ~p ~s ~p ~p:~p:~p GMT", [Wday, Day, MStr, Year, Hour, Mins, Secs]).

%% PNG  89 50 4E 47 0D 0A 1A 0A 
%% JPEG FF D8 FF E0

get_mime_type(<<Bytes:4/binary, _Rest/binary>> = _Bin) ->
    case Bytes of
	<<137, 80, 78, 71>> -> "image/png";
	<<255, 216, 255, 224>> -> "image/jpeg";		      
	_ -> "text/html; charset=utf-8"
    end.			   
    
get_file(FileName) ->
    {ok, Bin} = file:read_file(FileName),
    {get_mime_type(Bin), Bin}.

raw_internal_server_error() ->
    io_lib:format("HTTP/1.1 500 Internal Server Error
Server: erlang
Date: ~s
Content-Type: text/html; charset=utf-8
Content-Length: 23

Internal Server Error
", [header_date(calendar:universal_time())]).

raw_not_found() ->
    io_lib:format("HTTP/1.1 404 Not Found
Server: erlang
Data: ~s
Content-Type: text/html; charset=utf-8
Content-Length: 12

Not Found.
", [header_date(calendar:universal_time())]).

raw_response_ok({Len, LastModified, MimeType, Body}) ->
    io_lib:format("HTTP/1.1 200 OK
Server: erlang
Date: ~s
Content-Type: ~s
Last-modified: ~s
Content-Length: ~p

", [header_date(calendar:universal_time()), 
    MimeType, 
    header_date(LastModified), 
    Len])
 ++ Body.

rawget("/") ->
    rawget("/index.html");
rawget("/" ++ Object) -> 
    case filelib:find_file(Object, "views") of
	{ok, FileName} -> 
	    Len = filelib:file_size(FileName),
	    LastModified = hd(calendar:local_time_to_universal_time_dst(filelib:last_modified(FileName))),
	    {MimeType, Bin} = get_file(FileName),
	    raw_response_ok({Len, LastModified,  MimeType, Bin});
	{error, not_found} -> raw_not_found()
    end.

router(<<"GET ", RawHeaders/bitstring>>) -> 
    [Get|_Headers] = string:lexemes(bitstring_to_list(RawHeaders), [[$\r, $\n]]),
    [Object|_Http] = string:split(Get, " "),
    
    io:format("get ~p~n", [Object]),

    rawget(Object);

router(_) -> raw_internal_server_error().

handle_info({tcp, Socket, Msg}, #state{state=router} = S) ->
    io:format("http request: ~p~n", [Msg]),
    send(Socket, router(Msg), S), {stop, normal, S};

handle_info({tcp_closed, _Socket}, S) ->
    io:format("~p: closed socket~n", [S]),
    {noreply, S}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(normal, #state{socket=Socket}) ->
    ok = gen_tcp:close(Socket),
    ok;

terminate(Reason, #state{socket=Socket} = State) ->
    ok = gen_tcp:close(Socket),
    io:format("~p: terminate reason: ~p~n", [State, Reason]),
    ok.
