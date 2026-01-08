-module(daymate_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	SupFlags = #{
        strategy    => one_for_one,
        intensity   => 5,
        period      => 1
    },
    Children = [
        #{
            id => daymate_timer_api,
            start => {daymate_timer_api, start_link, []},
            restart => transient,
            type    => worker,
            modules => [daymate_timer_api]
        }
    ],
    {ok, {SupFlags, Children}}.
