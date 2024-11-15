-module(websocket_session).
-export([start/1, session_loop/1]).

%% Start a new WebSocket session
start(Socket) ->
    spawn(?MODULE, session_loop, [Socket]).

%% Session loop to receive and process messages
session_loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Frame} ->
            io:format("Received frame: ~p~n", [Frame]),
            %% Decode the WebSocket frame and handle the message
            case websocket_utils:decode_frame(Frame) of
                {ok, Payload} ->
                    io:format("Received message: ~s~n", [Payload]),
                    %% Handle the message making it a json response
                    Response = #{
                        status=> <<"success">>,
                        message=> <<"message received">>,
                        data=> Payload},
                    % Response = Payload,
                    %% Echo the message back for now
                    io:format("Sending response: ~p~n", [Response]),
                    gen_tcp:send(Socket, websocket_utils:encode_frame(Response)),
                    session_loop(Socket);
                _ ->
                    io:format("Failed to decode frame~n")
                    % gen_tcp:close(Socket)
            end;
        {error, closed} ->
            io:format("Client disconnected~n"),
            ok
    end.
