-module(websocket_handshake).
-export([perform_handshake/1]).

perform_handshake(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} when is_tuple(Data) ->
            io:format("Unexpected data format: ~p~n", [Data]),
            {error, unexpected_format};
        {ok, Data} when is_binary(Data); is_list(Data) ->
            BinaryData = case is_binary(Data) of
                true -> Data;
                false -> list_to_binary(Data)
            end,
            % io:format("Received Handshake Data: ~p~n", [BinaryData]),
            case parse_websocket_handshake(BinaryData) of
                {ok, SecWebSocketKey} ->
                    SecWebSocketAccept = generate_accept_key(SecWebSocketKey),
                    % io:format("Sec-WebSocket-Accept: ~p~n", [SecWebSocketAccept]),
                    Response = [
                        "HTTP/1.1 101 Switching Protocols\r\n",
                        "Upgrade: websocket\r\n",
                        "Connection: Upgrade\r\n",
                        "Sec-WebSocket-Accept: ", SecWebSocketAccept, "\r\n\r\n"
                    ],
                    gen_tcp:send(Socket, list_to_binary(Response)),
                    {ok, SecWebSocketKey};
                {error, Reason} ->
                    io:format("Invalid handshake: ~p~n", [Reason]),
                    {error, invalid_handshake}
            end;
        {error, Reason} ->
            io:format("Handshake failed: ~p~n", [Reason]),
            {error, handshake_failed}
    end.

parse_websocket_handshake(Data) ->
    case binary:split(Data, <<"\r\n">>, [global]) of
        Lines ->
            find_websocket_key(Lines)
    end.

find_websocket_key([]) ->
    {error, missing_sec_websocket_key};
find_websocket_key([Line|Rest]) ->
    case binary:match(Line, <<"Sec-WebSocket-Key:">>) of
        {_, _} ->
            [_, Key] = binary:split(Line, <<": ">>),
            {ok, Key};
        nomatch ->
            find_websocket_key(Rest)
    end.

generate_accept_key(Key) ->
    BaseKey = <<Key/binary, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11">>,
    SHA1Hash = crypto:hash(sha, BaseKey),
    base64:encode(SHA1Hash).
