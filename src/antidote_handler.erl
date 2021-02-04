%% @author joao
%% @author Pedro Lopes
%% @doc @todo Add description to antidote.

-module(antidote_handler).

-define(LOCK_WAIT_TIME, 0).
-define(LOCK_WAIT_TIME_ES, 0).

-define(NODE, 'antidote@127.0.0.1').

-type key() :: atom().
% valid antidote_crdt types
-type crdt_type() ::
    antidote_crdt_counter_b |
    antidote_crdt_counter_pn |
    antidote_crdt_counter_fat |
    antidote_crdt_map_go |
    antidote_crdt_set_go |
    antidote_crdt_register_lww |
    antidote_crdt_map_rr |
    antidote_crdt_register_mv |
    antidote_crdt_set_aw |
    antidote_crdt_set_rw |
    antidote_crdt_flag_ew |
    antidote_crdt_flag_dw |
    antidote_crdt_index_p |
    antidote_crdt_index_s |
    antidote_crdt_secure_set_go |
    antidote_crdt_secure_set_aw |
    antidote_crdt_secure_set_rw |
    antidote_crdt_secure_register_lww |
    antidote_crdt_secure_register_mv |
    antidote_crdt_secure_map_go |
    antidote_crdt_secure_map_rr.

-type bucket() :: atom().
-type bound_object() :: {key(), crdt_type(), bucket()}.
-type op_name() :: atom().
-type op_param() :: term().
% check antidote project
-type vectorclock() :: term().
-type snapshot_time() :: vectorclock() | ignore.

-record(tx_id, {
    local_start_time :: clock_time(),
    server_pid :: atom() | pid()
}).

% check antidote project
-type txid() :: #tx_id{}.
-type clock_time() :: non_neg_integer().
-type reason() :: term().
-type properties() :: term() | [].

%% Filtering types
-type filter() :: [filter_content()].

-type filter_content() :: table_filter() | projection_filter() | conditions_filter().

-type table_name() :: atom() | list().
-type table_filter() :: {tables, [table_name()]}.

-type column_name() :: atom() | list().
-type projection_filter() :: {projection, [column_name()]}.

-type conditions_filter() :: {conditions, [term()]}.

%% ====================================================================
%% API functions
%% ====================================================================
-export([
    start_transaction/0,
    start_transaction/1,
    start_transaction/2,
    read_objects/2,
    commit_transaction/1,
    abort_transaction/1,
    update_objects/2,
    query_objects/2,
    get_locks/3
]).

-spec start_transaction() -> {ok, txid()} | {error, reason()}.
start_transaction() ->
    start_transaction(ignore, []).

-spec start_transaction(properties()) -> {ok, txid()} | {error, reason()}.
start_transaction(Props) ->
    start_transaction(ignore, Props).

-spec start_transaction(snapshot_time(), properties()) -> {ok, txid()} | {error, reason()}.
start_transaction(Snapshot, Props) ->
    antidote:start_transaction(Snapshot, Props).

-spec commit_transaction(txid()) -> {ok, vectorclock()} | {error, reason()}.
commit_transaction(TxId) ->
    antidote:commit_transaction(TxId).

-spec abort_transaction(txid()) -> ok | {error, reason()}.
abort_transaction(TxId) ->
    antidote:abort_transaction(TxId).

-spec read_objects([bound_object()] | bound_object(), txid()) -> {ok, [term()]}.
read_objects(Objects, TxId) when is_list(Objects) ->
    antidote:read_objects(Objects, TxId);
read_objects(Object, Ref) ->
    read_objects([Object], Ref).

-spec update_objects(
    [{bound_object(), op_name(), op_param()} | {bound_object(), {op_name(), op_param()}}] |
    bound_object(),
    txid()
) -> ok | {error, reason()}.
update_objects(Objects, TxId) when is_list(Objects) ->
    antidote:update_objects(Objects, TxId);
update_objects(Object, Ref) ->
    update_objects([Object], Ref).

-spec query_objects(filter(), txid()) -> {ok, [term()]} | {error, reason()}.
query_objects(Filter, TxId) ->
    antidote:query_objects(Filter, TxId).

-spec get_locks([key()], [key()], txid()) -> ok.
get_locks(SharedLocks, ExclusiveLocks, TxId) ->
    Res = antidote:get_locks(SharedLocks, ExclusiveLocks, TxId),
    case Res of
        {ok, _SnapshotTime} ->
            ok;
        {error, Reason} ->
            throw(Reason)
    end.
