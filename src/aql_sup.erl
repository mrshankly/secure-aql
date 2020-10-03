%%%-------------------------------------------------------------------
%% @doc aql top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(aql_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    ElliOpts = [{callback, aql_http_handler}, {port, 3002}],
    ElliSpec = {
        fancy_http,
        {elli, start_link, [ElliOpts]},
        permanent,
        5000,
        worker,
        [elli]
    },

    {ok, {{one_for_one, 5, 10}, [ElliSpec]}}.

%%====================================================================
%% Internal functions
%%====================================================================
