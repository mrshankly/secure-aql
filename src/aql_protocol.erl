-module(aql_protocol).

-behaviour(ranch_protocol).

-export([start_link/3]).
-export([init/3]).

-include_lib("kernel/include/logger.hrl").

-include("aql_pb.hrl").

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

handle_message(Socket, Transport, _Request = #'Request'{type = 'QUERY', query = Query}) ->
    Response =
        case aql:query(binary_to_list(Query)) of
            {ok, []} ->
                #'QueryResponse'{response = <<"ok">>};
            {ok, Result} ->
                #'QueryResponse'{response = list_to_binary(Result)};
            {ok, [], _Transaction} ->
                #'QueryResponse'{response = <<"ok">>};
            {ok, Result, _Transaction} ->
                #'QueryResponse'{response = list_to_binary(Result)};
            {error, Reason} ->
                #'QueryResponse'{error = term_to_binary(Reason)}
        end,
    Transport:send(Socket, aql_pb:encode_msg(Response));
handle_message(_Socket, _Transport, Request) ->
    ?LOG_INFO("unknown request: ~p", [Request]),
    ok.
