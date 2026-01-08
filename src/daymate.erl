-module(daymate).
-export([start/0]).

start() ->
    case application:ensure_all_started(?MODULE) of
        {ok, StartedApp} ->
            io:format("Started applications: ~p~n", [StartedApp]),
            ok;
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason])
    end.