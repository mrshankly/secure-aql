%% @author Joao
%% @author Pedro Lopes
%% @doc @todo Add description to select.

-module(select).

-include("parser.hrl").
-include("aql.hrl").
-include("types.hrl").

-define(CONDITION(FieldName, Comparator, Value), {FieldName, Comparator, Value}).
-define(CONJUNCTION, ?CONJUNCTIVE_KEY("AND")).
-define(DISJUNCTION, ?DISJUNCTIVE_KEY("OR")).
-define(FUNCTION(Name, Args), {func, Name, Args}).
-define(COLUMN(Name), {col, Name}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% ====================================================================
%% API functions
%% ====================================================================
-export([exec/3]).

-export([
    table/1,
    projection/1,
    where/1
]).

exec({Table, _Tables}, Select, TxId) ->
    %TName = table:name(Table),
    Cols = table:columns(Table),
    Projection = projection(Select),
    case validate_projection(Projection, column:s_names(Cols)) of
        {error, Msg} ->
            throw(Msg);
        {ok, ResProjection} ->
            Condition = where(Select),
            NewCondition = send_offset(Condition, Cols, []),
            Filter = prepare_filter(Table, ResProjection, NewCondition),
            case antidote_handler:query_objects(Filter, TxId) of
                {ok, Result} ->
                    FinalResult = apply_offset(Result, Cols, []),
                    {ok, FinalResult};
                {error, Msg} ->
                    {error, Msg}
            end
        %Keys = where:scan(TName, Condition, TxId),
        %case Keys of
        %	[] -> {ok, []};
        %	_Else ->
        %		{ok, Results} = antidote:read_objects(Keys, TxId),
        %		VisibleResults = filter_visible(Results, TName, Tables, TxId),
        %		ProjectionResult = project(Projection, VisibleResults, [], Cols),
        %		ActualRes = apply_offset(ProjectionResult, Cols, []),
        %		{ok, ActualRes}
        %end.
    end.

table({TName, _Projection, _Where}) -> TName.

projection({_TName, Projection, _Where}) -> Projection.

where({_TName, _Projection, Where}) -> Where.

%% ====================================================================
%% Private functions
%% ====================================================================
validate_projection(?PARSER_WILDCARD, Columns) ->
    {ok, Columns};
validate_projection(Projection, Columns) ->
    Invalid = lists:foldl(
        fun(ProjCol, AccInvalid) ->
            case lists:member(ProjCol, Columns) of
                true ->
                    AccInvalid;
                false ->
                    lists:append(AccInvalid, [ProjCol])
            end
        end,
        [],
        Projection
    ),
    case Invalid of
        [] ->
            {ok, Projection};
        _Else ->
            ErrMsg = lists:flatten(io_lib:format("Column ~p does not exist", [Invalid])),
            {error, ErrMsg}
    end.

send_offset(?PARSER_WILDCARD, _Cols, Acc) ->
    Acc;
send_offset([{disjunctive, _} = Cond | Conds], Cols, Acc) ->
    send_offset(Conds, Cols, lists:append(Acc, [Cond]));
send_offset([{conjunctive, _} = Cond | Conds], Cols, Acc) ->
    send_offset(Conds, Cols, lists:append(Acc, [Cond]));
send_offset([Condition | Conds], Cols, Acc) when is_list(Condition) ->
    NewAcc = send_offset(Condition, Cols, []),
    send_offset(Conds, Cols, lists:append(Acc, [NewAcc]));
send_offset([Condition | Conds], Cols, Acc) ->
    ?CONDITION(FieldName, Comparator, Value) = Condition,
    Column = column:s_get(Cols, FieldName),
    Constraint = column:constraint(Column),
    Type = column:type(Column),
    case {Type, Constraint} of
        {?AQL_COUNTER_INT, ?CHECK_KEY({_Key, ?COMPARATOR_KEY(Comp), Offset})} ->
            InvertComp =
                case Comp of
                    ?PARSER_LESSER -> invert_comparator(Comparator);
                    ?PARSER_LEQ -> invert_comparator(Comparator);
                    _ -> Comparator
                end,
            AQLCounterValue = bcounter:to_bcounter(Value, Offset, Comp),
            NewCond = ?CONDITION(FieldName, InvertComp, AQLCounterValue),
            send_offset(Conds, Cols, lists:append(Acc, [NewCond]));
        _Else ->
            send_offset(Conds, Cols, lists:append(Acc, [Condition]))
    end;
send_offset([], _Cols, Acc) ->
    Acc.

invert_comparator(?PARSER_GREATER) -> ?PARSER_LESSER;
invert_comparator(?PARSER_LESSER) -> ?PARSER_GREATER;
invert_comparator(?PARSER_GEQ) -> ?PARSER_LEQ;
invert_comparator(?PARSER_LEQ) -> ?PARSER_GEQ;
invert_comparator(Comp) -> Comp.

prepare_filter(Table, Projection, Conditions) ->
    VisibilityConds = visibility_condition(Table),
    NewConditions =
        case Conditions of
            [] -> VisibilityConds;
            Conditions -> lists:append([[Conditions], [?CONJUNCTION], VisibilityConds])
        end,

    Conjunctions = group_conjunctions(NewConditions),

    TableName = table:name(Table),
    TablesField = ?T_FILTER(tables, [TableName]),
    ProjectionField = ?T_FILTER(projection, Projection),

    ConditionsField = ?T_FILTER(conditions, Conjunctions),
    [TablesField, ProjectionField, ConditionsField].

%% The idea is to build additional conditions that concern visibility.
%% Intuitively, these conditions would be sent to the Antidote node.
%% Instead, what is really sent to Antidote is a function that
%% internally tries to approach the following visibility conditions:
%% - In an update-wins dependency policy:
%% 			(state(row.pk) <> d AND
%% 			 state(row.fk_col1) <> d AND
%% 			 state(row.fk_col2) <> d ...)
%% - In a delete-wins dependency policy:
%% 			(state(row.pk) <> d AND
%% 			 ref_state(row.fk_col1) <> d AND ref_version(row.fk_col1) = version(row.fk_col1) AND
%% 			 ref_state(row.fk_col2) <> d AND ref_version(row.fk_col2) = version(row.fk_col2) ...)
visibility_condition(Table) ->
    Policy = table:policy(Table),
    Rule = crp:get_rule(Policy),
    ShCols = lists:foldl(
        fun(?T_FK(FkName, _, FkTable, _, _), Acc) ->
            case length(FkName) of
                1 -> lists:append(Acc, [[?COLUMN(FkName), FkTable]]);
                _ -> Acc
            end
        end,
        [],
        table:shadow_columns(Table)
    ),
    Func = ?FUNCTION(assert_visibility, [?COLUMN('#st'), Rule, ShCols]),
    [{Func, ?PARSER_EQUALITY, true}].

% groups of elements
apply_offset([Result | Results], Cols, Acc) when is_list(Result) ->
    Result1 = apply_offset(Result, Cols, []),
    apply_offset(Results, Cols, [Result1 | Acc]);
% groups of columns
apply_offset([{{'#st', _T}, _} | Values], Cols, Acc) ->
    apply_offset(Values, Cols, Acc);
apply_offset([{{'#version', _T}, _} | Values], Cols, Acc) ->
    apply_offset(Values, Cols, Acc);
apply_offset([{{Key, Type}, V} | Values], Cols, Acc) ->
    Col = maps:get(Key, Cols),
    Cons = column:constraint(Col),
    case {Type, Cons} of
        {?CRDT_BCOUNTER_INT, ?CHECK_KEY({_Key, ?COMPARATOR_KEY(Comp), Offset})} ->
            AQLCounterValue = bcounter:from_bcounter(Comp, V, Offset),
            apply_offset(Values, Cols, [{Key, AQLCounterValue} | Acc]);
        _Else ->
            apply_offset(Values, Cols, [{Key, V} | Acc])
    end;
apply_offset([], _Cols, Acc) ->
    lists:reverse(Acc).

group_conjunctions(?PARSER_WILDCARD) ->
    [];
group_conjunctions(WhereClause) when is_list(WhereClause) ->
    {Conditions, Connectors} = separate_conditions(WhereClause, {[], []}),
    [First | Tail] = Conditions,
    case is_list(First) of
        true -> group_conjunctions(Tail, Connectors, [{sub, group_conjunctions(First)}], []);
        false -> group_conjunctions(Tail, Connectors, [First], [])
    end.

group_conjunctions([Comp | Tail], [{conjunctive, _} | Tail2], Curr, Final) ->
    Conj =
        case is_list(Comp) of
            true -> {sub, group_conjunctions(Comp)};
            false -> Comp
        end,
    group_conjunctions(Tail, Tail2, [Conj | Curr], Final);
group_conjunctions([Comp | Tail], [{disjunctive, _} | Tail2], Curr, Final) ->
    Conj =
        case is_list(Comp) of
            true -> {sub, group_conjunctions(Comp)};
            false -> Comp
        end,
    group_conjunctions(Tail, Tail2, [Conj], [lists:reverse(Curr) | Final]);
group_conjunctions([], [], Curr, Final) ->
    {disjunction, lists:reverse([lists:reverse(Curr) | Final])}.

separate_conditions([{conjunctive, _} = Conn | Tail], {Conditions, Connectors}) ->
    separate_conditions(Tail, {Conditions, [Conn | Connectors]});
separate_conditions([{disjunctive, _} = Conn | Tail], {Conditions, Connectors}) ->
    separate_conditions(Tail, {Conditions, [Conn | Connectors]});
separate_conditions([Cond | Tail], {Conditions, Connectors}) ->
    separate_conditions(Tail, {[Cond | Conditions], Connectors});
separate_conditions([], {Conditions, Connectors}) ->
    {lists:reverse(Conditions), lists:reverse(Connectors)}.

%%====================================================================
%% Eunit tests
%%====================================================================

-ifdef(TEST).

conjunction_test() ->
    DefaultComp = {attr, [{equality, ignore}], val},
    TestClause1 = [
        DefaultComp,
        {conjunctive, ignore},
        DefaultComp,
        {disjunctive, ignore},
        DefaultComp
    ],
    TestClause2 = [
        DefaultComp,
        {disjunctive, ignore},
        DefaultComp,
        {disjunctive, ignore},
        DefaultComp
    ],
    TestClause3 = [
        DefaultComp,
        {conjunctive, ignore},
        DefaultComp,
        {conjunctive, ignore},
        DefaultComp
    ],
    Res1 = group_conjunctions(TestClause1),
    Res2 = group_conjunctions(TestClause2),
    Res3 = group_conjunctions(TestClause3),
    ?assertEqual({disjunction, [[DefaultComp, DefaultComp], [DefaultComp]]}, Res1),
    ?assertEqual({disjunction, [[DefaultComp], [DefaultComp], [DefaultComp]]}, Res2),
    ?assertEqual({disjunction, [[DefaultComp, DefaultComp, DefaultComp]]}, Res3).

conjunction_parenthesis_test() ->
    DefaultComp = {attr, [{equality, ignore}], val},
    TestClause1 = [
        [
            DefaultComp,
            {conjunctive, ignore},
            DefaultComp,
            {disjunctive, ignore},
            DefaultComp
        ],
        {conjunctive, ignore},
        DefaultComp
    ],
    Res1 = group_conjunctions(TestClause1),
    Expected = {
        disjunction,
        [
            [
                {sub,
                    {
                        disjunction,
                        [[DefaultComp, DefaultComp], [DefaultComp]]
                    }},
                DefaultComp
            ]
        ]
    },
    ?assertEqual(Expected, Res1).

-endif.
