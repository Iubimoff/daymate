-module(daymate_http_handler).

-export([init/2]).

init(Req0, State) ->
    try
        Path = cowboy_req:path(Req0),
        Qs = cowboy_req:qs(Req0),
        QsProplist = uri_string:dissect_query(Qs),
        QsMap = proplists:to_map(QsProplist),
        Method = cowboy_req:method(Req0),
        {ok, Body, _} = cowboy_req:read_body(Req0),
        Args = get_args_from_query_and_body(QsMap, Body),
        Response0 = dispatch(Method, Path, Args),
        {Code, Response} = parse_response(Response0),
        Req = cowboy_req:reply(Code, #{}, Response, Req0),
        {ok, Req, State}
    catch
        C:Reason:ST ->
            io:format("~n~nError:~n Class:~p~nReason~p~nStacktrace:~p~n", [C, Reason, ST]),
            {ok, Req0, State}
    end.

parse_response({ok, Body}) ->
    {200, jsx:encode(Body#{<<"status">> => <<"ok">>})};
parse_response({error, Body}) ->
    {400, jsx:encode(Body#{<<"status">> => <<"error">>})}.

get_args_from_query_and_body(QsMap, <<>>) ->
    QsMap;
get_args_from_query_and_body(QsMap, Body) ->
    Args = jsx:decode(Body, [return_maps, {labels, binary}]),
    maps:merge(Args, QsMap).

dispatch(<<"POST">>, <<"/timer">>, Args) ->
    daymate_timer_api:create_timer(Args);
dispatch(<<"GET">>, <<"/timer">>, Args) ->
    daymate_timer_api:get_timer(Args);
dispatch(<<"GET">>, <<"/timers">>, Args) ->
    daymate_timer_api:list_timers(Args);
dispatch(_, _, _) ->
    {error, undefined_path}.