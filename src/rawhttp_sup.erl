%%%-------------------------------------------------------------------
%% @doc rawhttp top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(rawhttp_sup).

-behaviour(supervisor).

-export([init/1, start_link/1, start_socket/0, socket_accept_pool/1]).

-define(PORT, 2224).
-define(POOL, 10).

-define(SERVER, ?MODULE).

start_link(Env) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Env]).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional

init([Env]) ->
    io:format("~p~n", [Env]),
    Port = proplists:get_value(port, Env, ?PORT),
    Pool = proplists:get_value(pool, Env, ?POOL),

    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 60,
                 period => 3600},
    case gen_tcp:listen(Port, [{active,once}, binary]) of
	{error,eaddrinuse} -> io:format("port already in use, stopping...\n"), init:stop();
	{ok, ListenSocket} -> 
	    %% start accept listener process pool to handle multiple connections at the same time
	    spawn_link(?MODULE, socket_accept_pool, [Pool]), 
	    %% pass tcp listener to gen_server child process
	    ChildSpecs = [#{id => server_tcp_child,
			    start => {rawhttp_server, start_link, [ListenSocket]}, 
			    restart => temporary,
			    modules => [rawhttp_server]
			   }], 
	    {ok, {SupFlags, ChildSpecs}}
    end.


%% internal functions
start_socket() ->
    supervisor:start_child(?MODULE, []).

socket_accept_pool(Pool) ->
    [start_socket() || _ <- lists:seq(1, Pool)].
