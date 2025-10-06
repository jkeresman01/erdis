%%%===================================================================
%%% @doc Minimal RESP (Redis Serialization Protocol) implementation.
%%%===================================================================
-module(erdis_protocol).
-export([decode/1, encode/1]).

-define(CRLF, <<"\r\n">>).

%%%===================================================================
%%% Decode RESP into list of binaries
%%%===================================================================

decode(Bin) when is_binary(Bin) ->
    case Bin of
        <<"*", Rest/binary>> ->
            {Count, AfterCount} = take_integer(Rest),
            {Args, _} = decode_bulk_args(Count, AfterCount, []),
            Args;
        _ ->
            %% Plain text fallback
            Parts = binary:split(Bin, <<" ">>, [global]),
            [binary:trim(P) || P <- Parts]
    end.

decode_bulk_args(0, After, Acc) ->
    {lists:reverse(Acc), After};
decode_bulk_args(N, Bin, Acc) when N > 0 ->
    <<"$", Rest/binary>> = Bin,
    {Len, AfterLen} = take_integer(Rest),
    <<Arg:Len/binary, "\r\n", Tail/binary>> = AfterLen,
    decode_bulk_args(N - 1, Tail, [Arg | Acc]).

%%%===================================================================
%%% Encode server responses (safe)
%%%===================================================================

encode(Value) ->
    try do_encode(Value)
    catch
        Class:Reason:Stack ->
            io:format("Encode failed (~p:~p) for value: ~p~nStack: ~p~n",
                      [Class, Reason, Value, Stack]),
            <<"-ERR internal encoding failure\r\n">>
    end.

do_encode({ok, <<"OK">>}) ->
    <<"+OK\r\n">>;

do_encode({ok, Bin}) when is_binary(Bin) ->
    LenBin = integer_to_binary(byte_size(Bin)),
    <<"$", LenBin/binary, "\r\n", Bin/binary, "\r\n">>;

do_encode({ok, Term}) ->
    Bin = iolist_to_binary(io_lib:format("~p", [Term])),
    LenBin = integer_to_binary(byte_size(Bin)),
    <<"$", LenBin/binary, "\r\n", Bin/binary, "\r\n">>;

do_encode({nil, _}) ->
    <<"$-1\r\n">>;

do_encode({error, Msg}) ->
    Bin = iolist_to_binary(io_lib:format("~p", [Msg])),
    <<"-", Bin/binary, "\r\n">>;

do_encode(Other) ->
    Bin = iolist_to_binary(io_lib:format("~p", [Other])),
    LenBin = integer_to_binary(byte_size(Bin)),
    <<"$", LenBin/binary, "\r\n", Bin/binary, "\r\n">>.

%%%===================================================================
%%% Internal helpers
%%%===================================================================

take_integer(Bin) ->
    [NumBin, Rest] = binary:split(Bin, ?CRLF),
    {binary_to_integer(NumBin), Rest}.

