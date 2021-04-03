%% @author Joao
%% @author Pedro Lopes
%% @doc @todo Add description to index.

-module(index).

-include("aql.hrl").
-include("types.hrl").
-include("parser.hrl").

-define(PINDEX_CRDT, antidote_crdt_index_p).
-define(SINDEX_CRDT, antidote_crdt_index_s).
-define(INDEX_ENTRY_DT, antidote_crdt_register_lww).
-define(ITAG_CRDT, antidote_crdt_map_go).
-define(ITAG_KEY_CRDT, antidote_crdt_register_mv).
-define(INDEX_PREFIX, "#_").
-define(SINDEX_PREFIX, "#2i_").
-define(TAG_TOKEN, "#__").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([exec/3]).
-export([
    index/1,
    table/1,
    cols/1
]).

-export([primary_index/2, secondary_index/3]).

-export([
    p_keys/2,
    s_keys/3,
    s_keys_formatted/3,
    p_name/1,
    s_name/2,
    p_put/1, p_put/2,
    lookup_index/2
]).

-export([
    tag_name/2,
    tag_key/2,
    tag_subkey/1,
    tag/5,
    tag_read/4
]).

exec({Table, _Tables}, Props, TxId) ->
    %IndexName = index(Props),
    TableName = table(Props),
    IndexCols = cols(Props),
    TIndexes = table:indexes(Table),
    %case has_index(TIndexes, IndexName) of
    %  true ->
    %    throw(lists:concat(["The index ", IndexName, " already exists on table ", TableName]));
    %  _Else ->
    %    ok
    %end,
    case check_keys(Table, IndexCols) of
        [] ->
            ok;
        List ->
            ErrorMsg = io_lib:format("The columns ~p do not exist in table ~p", [List, TableName]),
            throw(lists:flatten(ErrorMsg))
    end,
    Table2 = set_table_index(lists:append(TIndexes, [Props]), Table),
    TableUpdate = table:create_table_update(Table2),
    ok = antidote_handler:update_objects(TableUpdate, TxId).

index({Name, _TName, _Cols}) -> Name.

table({_Name, TName, _Cols}) -> TName.

cols({_Name, _TName, Cols}) -> Cols.

primary_index(TName, TxId) ->
    BoundObject = crdt:create_bound_object(p_name(TName), ?PINDEX_CRDT, ?METADATA_BUCKET),
    {ok, [Res]} = antidote_handler:read_objects(BoundObject, TxId),
    Res.

secondary_index(TName, IndexName, TxId) ->
    BoundObject = crdt:create_bound_object(
        s_name(TName, IndexName),
        ?SINDEX_CRDT,
        ?METADATA_BUCKET
    ),
    {ok, [Res]} = antidote_handler:read_objects(BoundObject, TxId),
    Res.

p_keys(TName, TxId) ->
    BoundObject = crdt:create_bound_object(p_name(TName), ?PINDEX_CRDT, ?METADATA_BUCKET),
    {ok, [Res]} = antidote_handler:read_objects(BoundObject, TxId),
    lists:map(
        fun({Key, BoundObj}) ->
            {utils:to_atom(Key), BoundObj}
        end,
        Res
    ).

%% Reads a secondary index
s_keys(TName, IndexName, TxId) ->
    BoundObject = crdt:create_bound_object(
        s_name(TName, IndexName),
        ?SINDEX_CRDT,
        ?METADATA_BUCKET
    ),
    {ok, [Res]} = antidote_handler:read_objects(BoundObject, TxId),
    Res.

s_keys_formatted(TName, IndexName, TxId) ->
    Tables = table:read_tables(TxId),
    Table = table:lookup(TName, Tables),
    IndexData = s_keys(TName, IndexName, TxId),
    %% todo support more than one column
    {_IndexName, _TableName, [Column]} = lookup_index(IndexName, Table),
    Col = column:s_get(Table, Column),
    Type = column:type(Col),
    Cons = column:constraint(Col),
    case {Type, Cons} of
        {?AQL_COUNTER_INT, ?CHECK_KEY({_Key, ?COMPARATOR_KEY(Comp), Offset})} ->
            lists:map(
                fun({EntryKey, EntryVal}) ->
                    AQLCounterValue = bcounter:from_bcounter(Comp, EntryKey, Offset),
                    {AQLCounterValue, EntryVal}
                end,
                IndexData
            );
        _Else ->
            IndexData
    end.

p_name(TName) ->
    TNameStr = utils:to_list(TName),
    NameStr = lists:concat([?INDEX_PREFIX, TNameStr]),
    list_to_atom(NameStr).

%% Builds the name of a secondary index
s_name(TName, IndexName) ->
    TNameStr = utils:to_list(TName),
    INameStr = utils:to_list(IndexName),
    NameStr = lists:concat([?SINDEX_PREFIX, TNameStr, ".", INameStr]),
    list_to_atom(NameStr).

p_put({Key, _Map, TName} = ObjBoundKey) ->
    BoundObject = crdt:create_bound_object(p_name(TName), ?PINDEX_CRDT, ?METADATA_BUCKET),
    AssignKey = crdt:assign_lww(Key),
    crdt:map_update(BoundObject, {?INDEX_ENTRY_DT, ObjBoundKey, AssignKey}).

p_put(Key, TxId) ->
    ok = antidote_handler:update_objects(p_put(Key), TxId).

lookup_index(IndexName, Table) ->
    Indexes = table:indexes(Table),
    IndexNameAtom = utils:to_atom(IndexName),
    lists:keyfind(IndexNameAtom, 1, Indexes).

tag_name(TName, Column) ->
    {TName, Column}.

tag_key(TName, Column) ->
    Key = tag_name(TName, Column),
    crdt:create_bound_object(Key, ?ITAG_CRDT, ?METADATA_BUCKET).

tag_subkey(CName) ->
    ?MAP_KEY(CName, ?ITAG_KEY_CRDT).

tag(TName, Column, Value, ITag) ->
    BoundObject = tag_key(TName, Column),
    MapOp = crdt:assign_lww(ITag),
    crdt:single_map_update(BoundObject, Value, ?ITAG_KEY_CRDT, MapOp).

tag(TName, Column, Value, ITag, TxId) ->
    antidote_handler:update_objects(tag(TName, Column, Value, ITag), TxId).

tag_read(TName, CName, Value, TxId) ->
    Key = tag_key(TName, CName),
    {ok, [Map]} = antidote_handler:read_objects(Key, TxId),
    SubKey = tag_subkey(Value),
    proplists:get_value(SubKey, Map).

%% ====================================================================
%% Private functions
%% ====================================================================

%has_index(Indexes, IndexName) ->
%  lists:keymember(IndexName, 1, Indexes).

check_keys(Table, Cols) ->
    TCols = column:s_names(Table),
    lists:filter(fun(Col) -> not lists:member(Col, TCols) end, Cols).

set_table_index(Idx, ?T_TABLE(Name, Policy, Cols, SCols, _Idx, PartCol)) ->
    ?T_TABLE(Name, Policy, Cols, SCols, Idx, PartCol).

%%====================================================================
%% Eunit tests
%%====================================================================

-ifdef(TEST).

name_test() ->
    Expected = '#_Test',
    ?assertEqual(Expected, p_name("Test")),
    ?assertEqual(Expected, p_name('Test')).

put_test() ->
    BoundObject = crdt:create_bound_object(key, map, test),
    Update = {?INDEX_ENTRY_DT, BoundObject, crdt:assign_lww(key)},
    Expected = crdt:map_update({'#_test', ?PINDEX_CRDT, ?METADATA_BUCKET}, Update),
    ?assertEqual(Expected, p_put(BoundObject)).

tag_name_test() ->
    ?assertEqual({test, id}, tag_name(test, id)).

tag_test() ->
    BoundObject = crdt:create_bound_object({test, id}, ?ITAG_CRDT, ?METADATA_BUCKET),
    ExpectedOp = crdt:assign_lww(ipa:touch_cascade()),
    Expected = crdt:single_map_update(BoundObject, "Sam", ?ITAG_KEY_CRDT, ExpectedOp),
    ?assertEqual(Expected, tag(test, id, "Sam", ipa:touch_cascade())).

-endif.
