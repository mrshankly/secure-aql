-module(aql_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-include_lib("kernel/include/logger.hrl").

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupervisorFlags = #{
        strategy => one_for_one,
        intensity => 1,
        period => 5
    },

    ListenerSpec = ranch:child_spec(
        aql_listener,
        ranch_tcp,
        [{port, 8321}],
        aql_protocol,
        []
    ),

    {ok, {SupervisorFlags, [ListenerSpec]}}.
