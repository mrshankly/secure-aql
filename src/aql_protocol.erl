-module(aql_protocol).

-behaviour(ranch_protocol).

-export([start_link/3]).
-export([init/3]).

-include_lib("kernel/include/logger.hrl").

-include("aql.hrl").
-include("aql_pb.hrl").
-include("types.hrl").

start_link(Ref, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Transport, Opts]),
    {ok, Pid}.

init(Ref, Transport, _Opts = []) ->
    {ok, Socket} = ranch:handshake(Ref),
    loop(Socket, Transport).

loop(Socket, Transport) ->
    case Transport:recv(Socket, 0, infinity) of
        {ok, Data} ->
            Message = aql_pb:decode_msg(Data, 'Request'),
            handle_message(Socket, Transport, Message),
            loop(Socket, Transport);
        {error, closed} ->
            ok = Transport:close(Socket);
        {error, Reason} ->
            ?LOG_ERROR("receive error: ~p", [Reason]),
            ok = Transport:close(Socket)
    end.

handle_message(Socket, Transport, #'Request'{type = 'QUERY', query = Query, transaction = Transaction}) when Transaction /= <<>> ->
    Response = run_query(binary_to_term(Query), binary_to_term(Transaction)),
    Transport:send(Socket, aql_pb:encode_msg(Response));
handle_message(Socket, Transport, #'Request'{type = 'QUERY', query = Query}) ->
    Response = run_query(binary_to_term(Query)),
    Transport:send(Socket, aql_pb:encode_msg(Response));
handle_message(Socket, Transport, #'Request'{type = 'METADATA', tables = Tables}) ->
    Response = get_metadata(split_tables(Tables)),
    Transport:send(Socket, aql_pb:encode_msg(Response));
handle_message(Socket, Transport, #'Request'{
    type = 'QUERY_AND_METADATA',
    query = Query,
    tables = Tables,
    transaction = Transaction
}) when Transaction /= <<>> ->
    Response0 = run_query(binary_to_term(Query), binary_to_term(Transaction)),
    Response = get_metadata(Response0, split_tables(Tables)),
    Transport:send(Socket, aql_pb:encode_msg(Response));
handle_message(Socket, Transport, #'Request'{
    type = 'QUERY_AND_METADATA',
    query = Query,
    tables = Tables
}) ->
    Response = get_metadata(run_query(binary_to_term(Query)), split_tables(Tables)),
    Transport:send(Socket, aql_pb:encode_msg(Response));
handle_message(Socket, Transport, #'Request'{type = 'START_TRANSACTION'}) ->
    Response = case antidote_handler:start_transaction([{certify, dont_certify}]) of
        {ok, Transaction} ->
            #'StartTransactionResponse'{transaction = term_to_binary(Transaction)};
        {error, Reason} ->
            #'StartTransactionResponse'{transaction_error = term_to_binary(Reason)}
    end,
    Transport:send(Socket, aql_pb:encode_msg(Response));
handle_message(Socket, Transport, #'Request'{type = 'COMMIT_TRANSACTION', transaction = RawTransaction}) ->
    Transaction = binary_to_term(RawTransaction),
    Response = case antidote_handler:commit_transaction(Transaction) of
        {ok, _} ->
            #'ACTransactionResponse'{error = <<>>};
        {error, Reason} ->
            #'ACTransactionResponse'{error = term_to_binary(Reason)}
    end,
    Transport:send(Socket, aql_pb:encode_msg(Response));
handle_message(Socket, Transport, #'Request'{type = 'ABORT_TRANSACTION', transaction = RawTransaction}) ->
    Transaction = binary_to_term(RawTransaction),
    Response = case antidote_handler:abort_transaction(Transaction) of
        ok ->
            #'ACTransactionResponse'{error = <<>>};
        {error, Reason} ->
            #'ACTransactionResponse'{error = term_to_binary(Reason)}
    end,
    Transport:send(Socket, aql_pb:encode_msg(Response));
handle_message(_Socket, _Transport, Request) ->
    ?LOG_INFO("unknown request: ~p", [Request]),
    ok.

split_tables(TableNames) ->
    lists:map(fun binary_to_atom/1, string:split(TableNames, ",", all)).

run_query(Query) ->
    {ok, Transaction} = antidote_handler:start_transaction([{certify, dont_certify}]),
    Response = run_query(#'Response'{}, Query, Transaction),
    antidote_handler:commit_transaction(Transaction),
    Response.

run_query(Query, Transaction) ->
    run_query(#'Response'{}, Query, Transaction).

run_query(Response, Query, Transaction) ->
    case aqlparser:execute_query(Query, Transaction) of
        {ok, []} ->
            Response#'Response'{query = term_to_binary(ok)};
        {ok, Result} ->
            Response#'Response'{query = term_to_binary(Result)};
        {ok, [], _Transaction} ->
            Response#'Response'{query = term_to_binary(ok)};
        {ok, Result, _Transaction} ->
            Response#'Response'{query = term_to_binary(Result)};
        {error, Reason} ->
            Response#'Response'{query_error = term_to_binary(Reason)};
        {error, Reason, _Transaction} ->
            Response#'Response'{query_error = term_to_binary(Reason)}
    end.

get_metadata(TableNames) ->
    get_metadata(#'Response'{}, TableNames).

get_metadata(Response, TableNames) ->
    {ok, Transaction} = antidote_handler:start_transaction([{certify, dont_certify}]),
    Tables = table:read_tables(Transaction),
    antidote_handler:commit_transaction(Transaction),

    GetColumnsMetadata = fun(TableName) ->
        case proplists:get_value(?MAP_KEY(TableName, ?META_CRDT_TYPE), Tables) of
            undefined ->
                undefined;
            Table when ?is_table(Table) ->
                {TableName, table:columns(Table)}
        end
    end,

    AllMetadata = lists:map(GetColumnsMetadata, TableNames),
    Metadata = lists:filter(fun(Table) -> Table =/= undefined end, AllMetadata),

    Response#'Response'{metadata = erlang:term_to_binary(Metadata)}.
