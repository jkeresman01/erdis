%%%-------------------------------------------------------------------
%% @doc erdis top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(erdis_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    Children = [
        {erdis_store, {erdis_store, start_link, []},
         permanent, 5000, worker, [erdis_store]},
        {erdis_server, {erdis_server, start_link, []},
         permanent, 5000, worker, [erdis_server]}
    ],
    {ok, {{one_for_one, 5, 10}, Children}}.

%% internal functions
