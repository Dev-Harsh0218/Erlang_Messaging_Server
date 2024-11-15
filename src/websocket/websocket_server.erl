-module(websocket_server).

-export([start_link/1,accept_connections/1,handle_connection/1]).

%% Starting the websocket server on a Specified Port 
start_link(Port) ->
    {ok,ListenSocket} = gen_tcp:listen(Port,[binary,{packet,raw},{active,false},{reuseaddr,true}]),
    io:format("Server Started on Port ~p~n",[Port]),
    accept_connections(ListenSocket).


%% accepting connection for communication here
accept_connections(ListenSocket) ->
    {ok,Socket} = gen_tcp:accept(ListenSocket),
    spawn(?MODULE,handle_connection,[Socket]),
    accept_connections(ListenSocket).

handle_connection(Socket) ->
    case websocket_handshake:perform_handshake(Socket) of
        {ok, SecWebSocketKey} ->
            io:format("Handshake successful. SecWebSocketKey: ~p~n", [SecWebSocketKey]),
            io:format("converting to session here~n"),
            websocket_session:start(Socket);
        {error,_Reason} ->
            gen_tcp:close(Socket),
            io:format("Handshake failed: ~p~n", [_Reason])
    end,
    %% Here we can set up the WebSocket handshake and manage client data.
    io:format("Client connected: ~p~n", [Socket]).
    %% For now, we'll keep it simple and close the socket after accepting.