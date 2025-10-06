%%%===================================================================
%%% @doc Handles one client connection with RESP protocol
%%%===================================================================
-module(erdis_client).
-export([start/1]).

start(Socket) ->
    inet:setopts(Socket, [{active, false}]),
    loop(Socket).

loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            Args = erdis_protocol:decode(Data),
            Response = safe_execute(Args),
            Reply = erdis_protocol:encode(Response),
            gen_tcp:send(Socket, Reply),
            loop(Socket);
        {error, closed} ->
            io:format("Client disconnected.~n"),
            ok
    end.

safe_execute([]) ->
    {error, <<"ERR empty command">>};
safe_execute(Args) ->
    try erdis_command:execute(Args)
    catch
        _:_ -> {error, <<"ERR internal error">>}
    end.

