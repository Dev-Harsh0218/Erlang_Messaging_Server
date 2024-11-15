%%%-------------------------------------------------------------------
%% @doc messaging_server public API
%% @end
%%%-------------------------------------------------------------------

-module(messaging_server_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    io:format("Starting messaging_server~n"),
    messaging_server_sup:start_link().
    
stop(_State) ->
    ok.

%% internal functions
