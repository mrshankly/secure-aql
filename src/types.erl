-module(types).

-include("aql.hrl").

-include_lib("parser.hrl").

%to_aql/1,
-export([
    to_crdt/3,
    to_parser/1,
    to_insert_op/3,
    to_insert_op/2
]).

%%to_aql(?CRDT_INTEGER) -> ?AQL_INTEGER;
%%to_aql(?CRDT_BOOLEAN) -> ?AQL_BOOLEAN;
%%to_aql(?CRDT_BCOUNTER_INT) -> ?AQL_COUNTER_INT;
%%to_aql(?CRDT_COUNTER_INT) -> ?AQL_COUNTER_INT;
%%to_aql(?CRDT_VARCHAR) -> ?AQL_VARCHAR;
%%to_aql(Invalid) ->
%%  ErrorMsg = io_lib:format("No mapping available for: ~p", [Invalid]),
%%  throw(lists:flatten(ErrorMsg)).

to_crdt(Type, plain, Constraint) ->
    to_crdt(Type, Constraint);
to_crdt(Type, _EncryptionType, Constraint) ->
    to_scrdt(Type, Constraint).

to_crdt(?AQL_INTEGER, _) ->
    ?CRDT_INTEGER;
to_crdt(?AQL_BOOLEAN, _) ->
    ?CRDT_BOOLEAN;
to_crdt(?AQL_COUNTER_INT, {?CHECK_TOKEN, _}) ->
    ?CRDT_BCOUNTER_INT;
to_crdt(?AQL_COUNTER_INT, _) ->
    ?CRDT_COUNTER_INT;
to_crdt(?AQL_VARCHAR, _) ->
    ?CRDT_VARCHAR;
to_crdt(Invalid, _) ->
    ErrorMsg = io_lib:format("No CRDT mapping available for: ~p", [Invalid]),
    throw(lists:flatten(ErrorMsg)).

to_scrdt(?AQL_INTEGER, _) ->
    ?SCRDT_INTEGER;
to_scrdt(?AQL_BOOLEAN, _) ->
    ?SCRDT_BOOLEAN;
to_scrdt(?AQL_COUNTER_INT, {?CHECK_TOKEN, _}) ->
    ?SCRDT_BCOUNTER_INT;
to_scrdt(?AQL_COUNTER_INT, _) ->
    ?SCRDT_COUNTER_INT;
to_scrdt(?AQL_VARCHAR, _) ->
    ?SCRDT_VARCHAR;
to_scrdt(Invalid, _) ->
    ErrorMsg = io_lib:format("No SCRDT mapping available for: ~p", [Invalid]),
    throw(lists:flatten(ErrorMsg)).

to_parser(?AQL_INTEGER) ->
    ?PARSER_NUMBER_TOKEN;
to_parser(?AQL_BOOLEAN) ->
    ?PARSER_STRING_TOKEN;
to_parser(?AQL_COUNTER_INT) ->
    ?PARSER_NUMBER_TOKEN;
to_parser(?AQL_VARCHAR) ->
    ?PARSER_STRING_TOKEN;
to_parser(Invalid) ->
    ErrorMsg = io_lib:format("No mapping available for: ~p", [Invalid]),
    throw(lists:flatten(ErrorMsg)).

% to_insert_op(?AQL_INTEGER, _, OpParam) ->
%     crdt:set_integer(OpParam);
% to_insert_op(?AQL_BOOLEAN, _, OpParam) when is_atom(OpParam) ->
%     case OpParam of
%         true ->
%             crdt:enable_flag(?IGNORE_OP);
%         false ->
%             crdt:disable_flag(?IGNORE_OP)
%     end;
% to_insert_op(?AQL_COUNTER_INT, {?CHECK_TOKEN, _}, OpParam) ->
%     crdt:increment_bcounter(OpParam);
% to_insert_op(?AQL_COUNTER_INT, _, OpParam) ->
%     crdt:increment_counter(OpParam);
% to_insert_op(?AQL_VARCHAR, _, OpParam) ->
%     crdt:assign_lww(OpParam);
% to_insert_op(Invalid, _Constraint, _OpParam) ->
%     ErrorMsg = io_lib:format("No mapping available for: ~p", [Invalid]),
%     throw(lists:flatten(ErrorMsg)).

% % Since CRDT_INTEGER is a LWW register type, we can ignore this case.
% to_insert_op(?CRDT_INTEGER, OpParam) -> crdt:set_integer(OpParam);
% to_insert_op(?CRDT_BOOLEAN, OpParam) when is_atom(OpParam) ->
%     case OpParam of
%         true ->
%             crdt:enable_flag(?IGNORE_OP);
%         false ->
%             crdt:disable_flag(?IGNORE_OP)
%     end;
% to_insert_op(?CRDT_BCOUNTER_INT, OpParam) ->
%     crdt:increment_bcounter(OpParam);
% to_insert_op(?CRDT_COUNTER_INT, OpParam) ->
%     crdt:increment_counter(OpParam);
% to_insert_op(?CRDT_VARCHAR, OpParam) ->
%     crdt:assign_lww(OpParam);
% to_insert_op(Invalid, _OpParam) ->
%     ErrorMsg = io_lib:format("No mapping available for: ~p", [Invalid]),
%     throw(lists:flatten(ErrorMsg)).

to_insert_op(CrdtType, OpParam) ->
    to_insert_op(CrdtType, ignore, OpParam).

to_insert_op(antidote_crdt_register_lww, _Constraint, OpParam) ->
    crdt:assign_lww(OpParam);
to_insert_op(antidote_crdt_secure_register_lww, _Constraint, OpParam) ->
    crdt:assign_lww(OpParam);
to_insert_op(antidote_crdt_flag_ew, _Constraint, true) ->
    crdt:enable_flag(?IGNORE_OP);
to_insert_op(antidote_crdt_flag_ew, _Constraint, false) ->
    crdt:disable_flag(?IGNORE_OP);
to_insert_op(antidote_crdt_counter_b, _Constraint, OpParam) ->
    crdt:increment_bcounter(OpParam);
to_insert_op(antidote_crdt_counter_pn, _Constraint, OpParam) ->
    crdt:increment_counter(OpParam);
to_insert_op(antidote_crdt_secure_counter_pn, _Constraint, _OpParam) ->
    throw("TODO");
to_insert_op(Invalid, _Constraint, _OpParam) ->
    ErrorMsg = io_lib:format("No mapping available for: ~p", [Invalid]),
    throw(lists:flatten(ErrorMsg)).
