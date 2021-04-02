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

    PbPort = application:get_env(aql, aql_pb_port, 8321),
    ListenerSpec = ranch:child_spec(
        aql_listener,
        ranch_tcp,
        [{port, PbPort}],
        aql_protocol,
        []
    ),

    HttpPort = application:get_env(aql, aql_http_port, 8322),
    ElliOpts = [{callback, aql_http_handler}, {port, HttpPort}],
    ElliSpec = {
        fancy_http,
        {elli, start_link, [ElliOpts]},
        permanent,
        5000,
        worker,
        [elli]
    },

    {ok, {SupervisorFlags, [ListenerSpec, ElliSpec]}}.
