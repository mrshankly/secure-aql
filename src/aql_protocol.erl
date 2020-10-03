-module(aql_protocol).

-behaviour(ranch_protocol).

-export([start_link/3]).
-export([init/3]).

-include_lib("kernel/include/logger.hrl").

start_link(Ref, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Transport, Opts]),
    {ok, Pid}.

init(Ref, Transport, _Opts = []) ->
    {ok, Socket} = ranch:handshake(Ref),
    loop(Socket, Transport).

loop(Socket, Transport) ->
    case Transport:recv(Socket, 0, infinity) of
        {ok, _Data} ->
            Transport:send(Socket, <<"GOT IT!!!\n">>),
            loop(Socket, Transport);
        {error, closed} ->
            ok = Transport:close(Socket);
        {error, Reason} ->
            ?LOG_ERROR("ranch: ~p", [Reason]),
            ok = Transport:close(Socket)
    end.
