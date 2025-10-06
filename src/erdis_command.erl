%%%===================================================================
%%% @doc Command executor for Erdis
%%%===================================================================
-module(erdis_command).
-export([execute/1]).

%%%===================================================================
%%% @doc Execute a command represented as a list of binaries.
%%% Example: ["SET", "Milica", "Krpotich"]
%%%===================================================================

execute([<<"SET">>, Key, Value]) ->
    erdis_store:set(Key, Value),
    {ok, <<"OK">>};

execute([<<"GET">>, Key]) ->
    case erdis_store:get(Key) of
        undefined ->
            {nil, Key};
        Value when is_binary(Value) ->
            {ok, Value};
        Value when is_list(Value) ->
            {ok, list_to_binary(Value)};
        Value ->
            Bin = list_to_binary(io_lib:format("~p", [Value])),
            {ok, Bin}
    end;

execute([<<"DEL">>, Key]) ->
    erdis_store:del(Key),
    {ok, <<"OK">>};

execute([<<"FLUSHALL">>]) ->
    erdis_store:flush(),
    {ok, <<"OK">>};

execute([<<"KEYS">>]) ->
    {ok, maps:keys(sys:get_state(erdis_store))};

execute([<<"COMMAND">>]) ->
    {ok, <<"OK">>};

execute([]) ->
    {error, <<"ERR empty command">>};

execute(Unknown) ->
    {error, io_lib:format("ERR unknown command: ~p", [Unknown])}.

