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

handle_message(Socket, Transport, #'Request'{type = 'QUERY', query = Query}) ->
    Response = run_query(binary_to_list(Query)),
    Transport:send(Socket, aql_pb:encode_msg(Response));
handle_message(Socket, Transport, #'Request'{type = 'METADATA', tables = Tables}) ->
    Response = get_metadata(split_tables(Tables)),
    Transport:send(Socket, aql_pb:encode_msg(Response));
handle_message(Socket, Transport, #'Request'{
    type = 'QUERY_AND_METADATA',
    query = Query,
    tables = Tables
}) ->
    Response = get_metadata(run_query(binary_to_list(Query)), split_tables(Tables)),
    Transport:send(Socket, aql_pb:encode_msg(Response));
handle_message(_Socket, _Transport, Request) ->
    ?LOG_INFO("unknown request: ~p", [Request]),
    ok.

split_tables(TableNames) ->
    lists:map(fun binary_to_atom/1, string:split(TableNames, ",", all)).

run_query(Query) ->
    run_query(#'Response'{}, Query).

run_query(Response, Query) ->
    case aql:query(binary_to_list(Query)) of
        {ok, []} ->
            Response#'Response'{query = <<"ok">>};
        {ok, Result} ->
            Response#'Response'{query = list_to_binary(Result)};
        {ok, [], _Transaction} ->
            Response#'Response'{query = <<"ok">>};
        {ok, Result, _Transaction} ->
            Response#'Response'{query = list_to_binary(Result)};
        {error, Reason} ->
            Response#'Response'{query_error = term_to_binary(Reason)};
        {error, Reason, _Transaction} ->
            Response#'Response'{query_error = term_to_binary(Reason)}
    end.

get_metadata(TableNames) ->
    get_metadata(#'Response'{}, TableNames).

get_metadata(Response, TableNames) ->
    {ok, Transaction} = antidote_handler:start_transaction(),
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
