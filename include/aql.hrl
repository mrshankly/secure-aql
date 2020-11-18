-define(METADATA_BUCKET, aql_metadata).
-define(MAP_KEY(Key, Type), {Key, Type}).
-define(BOUND_OBJECT(Key, Crdt, Bucket), {Key, Crdt, Bucket}).

-define(TABLE_META, ?BOUND_OBJECT('#tables', antidote_crdt_map_go, ?METADATA_BUCKET)).
-define(META_CRDT_TYPE, antidote_crdt_register_lww).

-define(IGNORE_OP, ignore).

-define(ADD_WINS, add).
-define(REMOVE_WINS, remove).
-define(NO_CONCURRENCY, noconcurrency).

% column
-define(C_NAMES, {names}).
-define(C_PK, {pk}).

% Encryption types.
-define(AQL_ENCRYPTED, encrypted).
-define(AQL_DT_ENCRYPTED, deterministic_encrypted).
-define(AQL_OP_ENCRYPTED, order_preserving_encrypted).
-define(AQL_HM_ENCRYPTED, homomorphic_encrypted).

% AQL -> CRDT mappings
-define(AQL_INTEGER, integer).
-define(CRDT_INTEGER, antidote_crdt_register_lww).
-define(SCRDT_INTEGER, antidote_crdt_secure_register_lww).

-define(AQL_VARCHAR, varchar).
-define(CRDT_VARCHAR, antidote_crdt_register_lww).
-define(SCRDT_VARCHAR, antidote_crdt_secure_register_lww).

-define(AQL_BOOLEAN, boolean).
-define(CRDT_BOOLEAN, antidote_crdt_flag_ew).
-define(SCRDT_BOOLEAN, antidote_crdt_secure_register_lww).

-define(AQL_COUNTER_INT, counter_int).
-define(CRDT_BCOUNTER_INT, antidote_crdt_counter_b).
-define(SCRDT_BCOUNTER_INT, antidote_crdt_counter_b).
-define(CRDT_COUNTER_INT, antidote_crdt_counter_pn).
-define(SCRDT_COUNTER_INT, antidote_crdt_secure_counter_pn).

% types
-export_type([input/0, queries/0, queryResult/0]).

-type input() :: input_str() | input_file().
-type input_str() :: {str, list()}.
-type input_file() :: {file, term()}.

-type queries() :: [aqlquery()].
-type aqlquery() ::
    create_query() |
    insert_query() |
    update_query() |
    select_query().

-type create_query() :: {create, create_query_props()}.
-type create_query_props() :: [create_policy() | create_name() | create_keys()].
%incomplete
-type create_name() :: {name, term()}.
%incomplete
-type create_policy() :: {table_policy, term()}.
-type create_keys() :: {keys, keys_list()}.
%incomplete
-type keys_list() :: term().

-type insert_query() :: {insert, insert_query_props()}.
-type update_query() :: {update, update_query_props()}.
-type select_query() :: {select, select_query_props()}.
-type queryResult() :: term().

%incomplete
-type insert_query_props() :: term().
%incomplete
-type update_query_props() :: term().
%incomplete
-type select_query_props() :: term().
