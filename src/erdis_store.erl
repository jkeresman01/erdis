%%%===================================================================
%%% @doc Erdis key-value store (core in-memory state process)
%%%===================================================================
-module(erdis_store).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([get/1]).
-export([set/2]).
-export([del/1]).
-export([flush/0]).

%% gen_server callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, #{}, []).

set(Key, Value) ->
    gen_server:cast(?SERVER, {set, Key, Value}).

get(Key) ->
    gen_server:call(?SERVER, {get, Key}).

del(Key) ->
    gen_server:cast(?SERVER, {del, Key}).

flush() ->
    gen_server:cast(?SERVER, flush).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(State) ->
    {ok, State}.

handle_call({get, Key}, _From, State) ->
    K = to_binary(Key),
    case maps:get(K, State, undefined) of
        undefined ->
            {reply, undefined, State};
        V when is_binary(V) ->
            {reply, V, State};
        V ->
            {reply, to_binary(V), State}
    end;

handle_call(_Msg, _From, State) ->
    {reply, error, State}.

handle_cast({set, Key, Value}, State) ->
    K = to_binary(Key),
    V = to_binary(Value),
    {noreply, maps:put(K, V, State)};

handle_cast({del, Key}, State) ->
    K = to_binary(Key),
    {noreply, maps:remove(K, State)};

handle_cast(flush, _State) ->
    io:format("Flushing all keys.~n"),
    {noreply, #{}}; % reset to empty map

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Helpers
%%%===================================================================

to_binary(V) when is_binary(V) ->
    V;
to_binary(V) when is_list(V) ->
    list_to_binary(V);
to_binary(V) when is_atom(V) ->
    atom_to_binary(V, utf8);
to_binary(V) when is_integer(V) ->
    integer_to_binary(V);
to_binary(V) ->
    list_to_binary(io_lib:format("~p", [V])).
