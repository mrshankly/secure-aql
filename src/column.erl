-module(column).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("parser.hrl").
-include("aql.hrl").
-include("types.hrl").

-export([
    name/1,
    constraint/1,
    set_constraint/2,
    type/1,
    encryption_type/1,
    is_primary_key/1,
    is_default/1,
    is_foreign_key/1,
    is_restrict_fk/1,
    is_cascade_fk/1,
    is_check_valid/1
]).

-export([
    s_primary_key/1,
    s_filter_defaults/1,
    s_get/2, s_get/3,
    s_names/1
]).

%% ====================================================================
%% Column props functions
%% ====================================================================

name(?T_COL(Name, _, _, _)) -> Name.

constraint(?T_COL(_, _, _, Constraint)) -> Constraint.

set_constraint(Constraint, ?T_COL(Name, Type, Enc, _)) -> ?T_COL(Name, Type, Enc, Constraint).

type(?T_COL(_, Type, _, _)) -> Type.

encryption_type(?T_COL(_, _, EncryptionType, _)) -> EncryptionType.

is_primary_key(?T_COL(_, _, _, ?PRIMARY_TOKEN)) -> true;
is_primary_key(_) -> false.

is_default(?T_COL(_, _, _, ?DEFAULT_KEY(_V))) -> true;
is_default(_) -> false.

is_foreign_key(?T_COL(_, _, _, ?FOREIGN_KEY(_V))) -> true;
is_foreign_key(_) -> false.

is_restrict_fk(?T_COL(_, _, _, ?FOREIGN_KEY({_, _, ?RESTRICT_TOKEN}))) -> true;
is_restrict_fk(_) -> false.

is_cascade_fk(?T_COL(_, _, _, ?FOREIGN_KEY({_, _, ?CASCADE_TOKEN}))) -> true;
is_cascade_fk(_) -> false.

is_check_valid(?T_COL(ColName, _, _, ?CHECK_KEY({ColName, ?COMPARATOR_KEY(_), _}))) -> true;
is_check_valid(?T_COL(_ColName1, _, _, ?CHECK_KEY({_ColName2, ?COMPARATOR_KEY(_), _}))) -> false;
is_check_valid(_) -> true.

%% ====================================================================
%% Columns Utilities
%% ====================================================================

s_primary_key(Table) when ?is_table(Table) ->
    Columns = table:columns(Table),
    s_primary_key(Columns);
s_primary_key(Columns) ->
    PkNames = maps:get(?C_PK, Columns),
    lists:map(
        fun(PkName) ->
            maps:get(PkName, Columns)
        end,
        PkNames
    ).

s_filter_defaults(Table) when ?is_table(Table) ->
    s_filter_defaults(table:columns(Table));
s_filter_defaults(Columns) when is_list(Columns) ->
    lists:filter(fun is_default/1, Columns);
s_filter_defaults(Columns) ->
    maps:filter(fun(_K, V) -> is_default(V) end, Columns).

s_get(Table, Column) when ?is_table(Table) ->
    Columns = table:columns(Table),
    s_get(Columns, Column);
s_get(Columns, ColumnName) ->
    ErrMsg = lists:flatten(io_lib:format("Column ~p does not exist", [ColumnName])),
    s_get(Columns, ColumnName, ErrMsg).

s_get(Table, CName, ErrMsg) when ?is_table(Table) ->
    Columns = table:columns(Table),
    s_get(Columns, CName, ErrMsg);
s_get(Cols, CName, ErrMsg) ->
    Res = maps:find(CName, Cols),
    case Res of
        {ok, Column} ->
            Column;
        _Else ->
            throw(ErrMsg)
    end.

s_names(Table) when ?is_table(Table) ->
    s_names(table:columns(Table));
s_names(Cols) when is_map(Cols) ->
    maps:get(?C_NAMES, Cols).

%%====================================================================
%% Eunit tests
%%====================================================================

-ifdef(TEST).

pk() -> ?PRIMARY_TOKEN.

def() -> ?DEFAULT_KEY("Test").

fk() -> ?FOREIGN_KEY({"TName", "TCol", restrict}).

check() -> ?CHECK_KEY({"TCol", "<", 3}).

no() -> ?NO_CONSTRAINT.

name_test() ->
    Expected = test,
    ?assertEqual(Expected, name(?T_COL(Expected, a, a, a))).

constraint_test() ->
    Expected = check(),
    ?assertEqual(Expected, constraint(?T_COL(a, a, a, Expected))).

type_test() ->
    Expected = test,
    ?assertEqual(Expected, type(?T_COL(a, Expected, a, a))).

encryption_type_test() ->
    Expected = test,
    ?assertEqual(Expected, encryption_type(?T_COL(a, a, Expected, a))).

is_primary_key_test() ->
    ?assertEqual(true, is_primary_key(?T_COL(a, a, a, pk()))),
    ?assertEqual(false, is_primary_key(?T_COL(a, a, a, def()))),
    ?assertEqual(false, is_primary_key(?T_COL(a, a, a, fk()))),
    ?assertEqual(false, is_primary_key(?T_COL(a, a, a, check()))),
    ?assertEqual(false, is_primary_key(?T_COL(a, a, a, no()))).

is_default_test() ->
    ?assertEqual(false, is_default(?T_COL(a, a, a, pk()))),
    ?assertEqual(true, is_default(?T_COL(a, a, a, def()))),
    ?assertEqual(false, is_default(?T_COL(a, a, a, fk()))),
    ?assertEqual(false, is_default(?T_COL(a, a, a, check()))),
    ?assertEqual(false, is_default(?T_COL(a, a, a, no()))).

is_foreign_key_test() ->
    ?assertEqual(false, is_foreign_key(?T_COL(a, a, a, pk()))),
    ?assertEqual(false, is_foreign_key(?T_COL(a, a, a, def()))),
    ?assertEqual(true, is_foreign_key(?T_COL(a, a, a, fk()))),
    ?assertEqual(false, is_foreign_key(?T_COL(a, a, a, check()))),
    ?assertEqual(false, is_foreign_key(?T_COL(a, a, a, no()))).

-endif.
