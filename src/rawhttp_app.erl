%%%-------------------------------------------------------------------
%% @doc rawhttp public API
%% @end
%%%-------------------------------------------------------------------

-module(rawhttp_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Env = application:get_all_env(server),
    rawhttp_sup:start_link(Env).

stop(_State) ->
    ok.

%% internal functions
