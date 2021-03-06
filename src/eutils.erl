%%%-------------------------------------------------------------------
%%% @author joao
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. ago 2017 15:14
%%%-------------------------------------------------------------------
-module(eutils).

-author("joao").

-include("parser.hrl").

%% API
-export([create_table_aux/0]).

create_table_aux() ->
    {ok, Tokens, _} = scanner:string(
        "CREATE UPDATE-WINS TABLE Universities (WorldRank INT PRIMARY KEY, InstitutionId VARCHAR DEFAULT 'aaa', NationalRank COUNTER_INT CHECK (NationalRank > 5));"
    ),
    {ok, [?CREATE_CLAUSE(Table)]} = parser:parse(Tokens),
    table:prepare_table(Table, [], undefined).
