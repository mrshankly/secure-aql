-module(aql_http_handler).

-export([handle/2, handle_event/3]).

-include_lib("elli/include/elli.hrl").
-behaviour(elli_handler).

handle(Req, _Args) ->
    handle(Req#req.method, elli_request:path(Req), Req).

handle('POST', [<<"aql">>], Req) ->
    case elli_request:post_arg_decoded(<<"query">>, Req, <<"undefined">>) of
        <<"undefined">> ->
            {400, [], <<"No query parameter in POST request body!">>};
        Query ->
            case aql:query(binary_to_list(Query)) of
                {ok, Result} ->
                    Encoded = jsone:encode(utils:result_strings_to_binary(Result)),
                    {ok, [], Encoded};
                {ok, Result, _Tx} ->
                    Encoded = jsone:encode(utils:result_strings_to_binary(Result)),
                    {ok, [], Encoded};
                {error, Reason} ->
                    {500, [], utils:reason_to_binary(Reason)};
                {error, Reason, _} ->
                    {500, [], utils:reason_to_binary(Reason)}
            end
    end;
handle(_, _, _Req) ->
    {404, [], <<"Not Found">>}.

%% @doc Handle request events, like request completed, exception
%% thrown, client timeout, etc. Must return `ok'.
handle_event(_Event, _Data, _Args) ->
    ok.
