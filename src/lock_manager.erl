%%%-------------------------------------------------------------------
%%% @author pedrolopes
%%% @doc
%%%
%%% @end
%%% Created : 07. set 2018 13:46
%%%-------------------------------------------------------------------
-module(lock_manager).

-author("pedrolopes").

-include("aql.hrl").

%% API
-export([
    acquire_exclusive_lock/2,
    acquire_shared_lock/2
]).

acquire_exclusive_lock(ExclusiveLocks, TxId) when is_list(ExclusiveLocks) ->
    antidote_handler:get_locks([], ExclusiveLocks, TxId);
acquire_exclusive_lock(ExclusiveLock, TxId) ->
    acquire_exclusive_lock([ExclusiveLock], TxId).

acquire_shared_lock(SharedLocks, TxId) when is_list(SharedLocks) ->
    antidote_handler:get_locks(SharedLocks, [], TxId);
acquire_shared_lock(SharedLock, TxId) ->
    acquire_shared_lock([SharedLock], TxId).
