-module(daymate_timer_api).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/0, init/1, handle_info/2]).
-export([handle_call/3]).
-export([handle_cast/2]).

-export([create_timer/1]).
-export([list_timers/1]).

-define(CREATE, <<
    "INSERT INTO timers(id, created, date_end, duration, title, description) "
    "VALUES (?, ?, ?, ?, ?, ?)"
>>).

init() ->
    ok.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, #{}, []).

init(State) ->
    {ok, State}.

create_timer(Args) ->
    Title = maps:get(<<"title">>, Args, null),
    Description = maps:get(<<"description">>, Args, null),
    Hours = maps:get(<<"hours">>, Args, 0),
    Minutes = maps:get(<<"minutes">>, Args, 0),
    Seconds = maps:get(<<"seconds">>, Args, 0),
    case [Hours, Minutes, Seconds] of
        [0, 0, 0] ->
            {error, <<"Incorrect duration.">>};
        _ ->
            check_if_ws_process_exist_and_create(Hours, Minutes, Seconds, Title, Description)
    end.

check_if_ws_process_exist_and_create(Hours, Minutes, Seconds, Title, Description) ->
    case daymate_ws_handler:get_ws_process() of
        {ok, _} ->
            TimerId = list_to_binary(uuid:uuid_to_string(uuid:get_v4())),
            DurationSec = Hours * 3600 + Minutes * 60 + Seconds,
            Created = erlang:system_time(second),
            DateEnd = Created + DurationSec,
            Params = [TimerId, Created, DateEnd, DurationSec, Title, Description],
            _ = daymate_db:insert(?CREATE, Params),
            erlang:send_after(DurationSec * 1000, ?MODULE, {timer_fired, TimerId}),
            #{<<"status">> => <<"ok">>, <<"timer_id">> => TimerId};
        {error, no_process} ->
            #{
                <<"status">> => <<"error">>,
                <<"message">> => <<"Websocket doesn't connect.">>
            };
        {error, undefined_ets_table} ->
            ok = daymate_ws_handler:init(),
            check_if_ws_process_exist_and_create(Hours, Minutes, Seconds, Title, Description)
    end.

list_timers(Args) ->
    #{<<"key">> => <<"value">>}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({timer_fired, TimerId}, State) ->
    daymate_ws_handler:send(#{
        event => <<"timer_fired">>,
        timer_id => TimerId
    }),
    {noreply, State}.