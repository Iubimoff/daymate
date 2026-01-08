-module(daymate_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    ok = start_http(),
    ok = daymate_timer_api:init(),
    ok = daymate_clients:init(),
    ok = daymate_ws_handler:init(),
    daymate_sup:start_link().

start_http() ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/ws", daymate_ws_handler, []},
            {"/[...]", daymate_http_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(http,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    ok.

stop(_State) ->
	ok = cowboy:stop_listener(http).
