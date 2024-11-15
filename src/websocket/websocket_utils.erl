-module(websocket_utils).

-export([decode_frame/1, encode_frame/1, unmask/2]).

%% Decode a WebSocket frame for simple text messages
decode_frame(Frame) ->
    <<_Fin:1, _Rsv:3, Opcode:4, Mask:1, Initial_PayloadLen:7, Rest/binary>> = Frame,
    io:format("Frame details - FIN: ~p, RSV: ~p, Opcode: ~p, Mask: ~p, Initial Len: ~p~n", 
             [_Fin, _Rsv, Opcode, Mask, Initial_PayloadLen]),

    try
        {PayloadLen, RemainderData} = case Initial_PayloadLen of
            126 -> 
                <<Len:16, Rest2/binary>> = Rest,
                {Len, Rest2};
            127 ->
                <<Len:64, Rest2/binary>> = Rest,
                {Len, Rest2};
            _ -> 
                {Initial_PayloadLen, Rest}
        end,
        
        case Opcode of
            1 -> %% Text frame
                <<MaskingKey:32, PayloadData:PayloadLen/binary, _/binary>> = RemainderData,
                {ok, unmask(PayloadData, <<MaskingKey:32>>)};
            2 -> %% Binary frame
                {ok, RemainderData};
            8 -> %% Close frame
                {ok, close};
            9 -> %% Ping frame
                {ok, ping};
            10 -> %% Pong frame
                {ok, pong};
            _ -> %% Unknown opcode
                {error, unknown_opcode}
        end
    catch
        _:_ -> {error, invalid_frame}
    end.


%% Encode a text message as a WebSocket frame
encode_frame(Payload) ->
    PayloadBin = if
            is_list(Payload) -> list_to_binary(Payload);
            is_binary(Payload) -> Payload;
            is_map(Payload) ->
                % Use jsx:encode directly on the map
                jsx:encode(#{
                    status => maps:get(status, Payload),
                    message => maps:get(message, Payload),
                    data => maps:get(data, Payload)
                })
        end,
    PayloadLen = byte_size(PayloadBin),
    case PayloadLen of
        Len when Len =< 125 ->
            <<1:1, 0:3, 1:4, 0:1, PayloadLen:7, PayloadBin/binary>>;
        Len when Len =< 65535 ->
            <<1:1, 0:3, 1:4, 0:1, 126:7, Len:16, PayloadBin/binary>>;
        Len ->
            <<1:1, 0:3, 1:4, 0:1, 127:7, Len:64, PayloadBin/binary>>
    end.
%% Unmask a payload using the masking key
unmask(Payload, MaskingKey) ->
    unmask_payload(Payload, MaskingKey, <<>>).

unmask_payload(<<>>, _, Acc) ->
    Acc;
unmask_payload(<<Byte:8, Rest/binary>>, <<Key:8, KeyRest/binary>>, Acc) ->
    unmask_payload(Rest, <<KeyRest/binary, Key:8>>, <<Acc/binary, (Byte bxor Key):8>>).
