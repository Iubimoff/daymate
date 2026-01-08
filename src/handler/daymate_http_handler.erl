-module(daymate_http_handler).

-export([init/2]).

init(Req0, State) ->
    Path = cowboy_req:path(Req0),
    Qs = cowboy_req:qs(Req0),
    QsProplist = uri_string:dissect_query(Qs),
    QsMap = proplists:to_map(QsProplist),
    Method = cowboy_req:method(Req0),
    {ok, Body, _} = cowboy_req:read_body(Req0),
    Args = jsx:decode(Body, [return_maps, {labels, binary}]),
    Response0 = dispatch(Method, Path, Args, QsMap),
    Response = jsx:encode(Response0),
    Req = cowboy_req:reply(200, #{}, Response, Req0),
    {ok, Req, State}.

dispatch(<<"POST">>, <<"/timer">>, Args, QsMap) ->
    daymate_timer_api:create_timer(Args);
dispatch(<<"GET">>, <<"/timers">>, Args, QsMap) ->
    daymate_timer_api:list_timers();
dispatch(_, _, _, _) ->
    {error, undefined_path}.