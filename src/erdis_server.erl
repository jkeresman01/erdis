%%%===================================================================
%%% @doc Erdis TCP Server — listens on port 6379 and spawns clients
%%%===================================================================
-module(erdis_server).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([stop/0]).

%% gen_server callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define(PORT, 6379).

-record(state, {listen_socket}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, LSock} = gen_tcp:listen(?PORT,
        [binary, {packet, raw}, {active, false}, {reuseaddr, true}]),
    io:format("Erdis listening on port ~p~n", [?PORT]),
    spawn(fun() -> accept_loop(LSock) end),
    {ok, #state{listen_socket = LSock}}.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast(stop, State = #state{listen_socket = LSock}) ->
    gen_tcp:close(LSock),
    io:format("Erdis server stopped.~n"),
    {stop, normal, State};

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, #state{listen_socket = LSock}) ->
    catch gen_tcp:close(LSock),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

accept_loop(LSock) ->
    case gen_tcp:accept(LSock) of
        {ok, Socket} ->
            Pid = spawn(erdis_client, start, [Socket]),
            io:format("Client connected: ~p~n", [Pid]),
            accept_loop(LSock);
        {error, closed} ->
            io:format("Listener socket closed.~n"),
            ok
    end.

