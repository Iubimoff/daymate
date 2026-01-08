-module(daymate_ws_handler).
-behaviour(cowboy_websocket).

-export([get_ws_process/0]).
-export([websocket_init/1]).
-export([websocket_info/2]).
-export([init/0, init/2]).
-export([websocket_handle/2]).
-export([send/1]).

websocket_init(State) ->
    {ok, State}.

init() ->
    _ = ets:new(?MODULE, [named_table, public, set]),
    ok.

init(Req, State) ->
    {cowboy_websocket, Req, State, #{idle_timeout => 21600000}}.

websocket_handle({text, Data}, State) ->
    DecodedData = jsx:decode(Data),
    Type = maps:get(<<"type">>, DecodedData),
    Response = dispatch(Type, DecodedData),
    {[{text, jsx:encode(Response)}], State};
websocket_handle(_Any, State) ->
    {ok, State}.

dispatch(<<"init">>, Data) ->
    _ = ets:insert_new(?MODULE, {websocket_process, self()}),
    #{<<"status">> => <<"ok">>};
dispatch(_, Data) ->
    #{<<"status">> => <<"error">>, <<"message">> => <<"unknown_type">>}.

send(Message) ->
    {ok, Pid} = get_ws_process(),
    Pid ! {text, jsx:encode(Message)}.

websocket_info({text, Msg}, State) ->
    {reply, {text, Msg}, State}.

get_ws_process() ->
    case ets:lookup(?MODULE, websocket_process) of
        [{_, Pid}] ->
            {ok, Pid};
        [] ->
            {error, no_process};
        _ ->
            {error, undefined_ets_table}
    end.