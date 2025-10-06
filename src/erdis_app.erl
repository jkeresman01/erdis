%%%-------------------------------------------------------------------
%% @doc erdis public API
%% @end
%%%-------------------------------------------------------------------

-module(erdis_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    erdis_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
