-module(no_concurrency_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Test server callbacks
-export([
    suite/0,
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

-export([
    exclusive_lock/1,
    exclusive_lock_and_shared_lock/1,
    shared_lock/1,
    shared_lock_and_exclusive_lock/1
]).

% =====================
% CT callback functions
% =====================

suite() -> [].

all() ->
    [exclusive_lock, exclusive_lock_and_shared_lock, shared_lock, shared_lock_and_exclusive_lock].

init_per_suite(Config) ->
    aql:start(),
    {ok, [], _} = aql:query(
        "CREATE UPDATE-WINS TABLE Company(\n"
        "    Name VARCHAR PRIMARY KEY\n"
        ");"
    ),
    {ok, [], _} = aql:query(
        "CREATE UPDATE-WINS TABLE Employee(\n"
        "    Name VARCHAR PRIMARY KEY,\n"
        "    Employer VARCHAR FOREIGN KEY REFERENCES Company(Name)\n"
        ");"
    ),
    Config.

end_per_suite(_Config) ->
    aql:stop(),
    ok.

init_per_testcase(_Case, Config) ->
    {ok, [], _} = aql:query("INSERT INTO Company (Name) VALUES ('Company A');"),
    Config.

end_per_testcase(_Case, _Config) ->
    {ok, [], _} = aql:query("DELETE FROM Employee;"),
    {ok, [], _} = aql:query("DELETE FROM Company;"),
    ok.

% ==========
% Test cases
% ==========

exclusive_lock(_Config) ->
    {ok, _, T1} = aql:query("BEGIN TRANSACTION;"),
    {ok, [], T1} = aql:query("DELETE FROM Company WHERE Name = 'Company A';", T1),
    {ok, [{ok, commit_tx}], _} = aql:query("COMMIT TRANSACTION;", T1),

    {ok, [Companies], _} = aql:query("SELECT Name FROM Company;"),
    ?assertEqual([], Companies).

exclusive_lock_and_shared_lock(_Config) ->
    % Client 1, start a new transaction and delete the existing company, acquiring an exclusive lock.
    {ok, _, T1} = aql:query("BEGIN TRANSACTION;"),
    {ok, [], T1} = aql:query("DELETE FROM Company WHERE Name = 'Company A';", T1),

    Parent = self(),
    % Client 2 starts a new transaction before client 1 commits his. Client 2 tries to add a new employee
    % to the company client 1 is trying to delete. When client 2 tries to acquire a shared lock it should
    % fail because client 1 is holding an exclusive lock. Client 2 will be able to make progress when
    % client 1 releases the exclusive lock.
    {ok, _, T2} = aql:query("BEGIN TRANSACTION;"),
    spawn(fun() ->
        {ok, [], T2} = aql:query(
            "INSERT INTO Employee (Name, Employer) VALUES ('John Doe', 'Company A');",
            T2
        ),
        Parent ! done
    end),

    receive
        done ->
            throw("client 2 was able to make progress when it should not have")
    after 3000 ->
        % Back to client 1, release exclusive lock.
        {ok, [{ok, commit_tx}], _} = aql:query("COMMIT TRANSACTION;", T1)
    end,

    receive
        done ->
            {ok, [{ok, abort_tx}], _} = aql:query("ROLLBACK TRANSACTION;", T2)
    after 1000 -> throw("client 2 failed to make progress")
    end,

    {ok, [Companies], _} = aql:query("SELECT Name FROM Company;"),
    ?assertEqual([], Companies),
    {ok, [Employees], _} = aql:query("SELECT Name FROM Employee;"),
    ?assertEqual([], Employees).

shared_lock(_Config) ->
    % Client 1.
    {ok, _, T1} = aql:query("BEGIN TRANSACTION;"),
    {ok, [], T1} = aql:query(
        "INSERT INTO Employee (Name, Employer) VALUES ('John', 'Company A');",
        T1
    ),
    % Client 2.
    {ok, _, T2} = aql:query("BEGIN TRANSACTION;"),
    {ok, [], T2} = aql:query(
        "INSERT INTO Employee (Name, Employer) VALUES ('Bob', 'Company A');",
        T2
    ),
    % Back to client 1.
    {ok, [{ok, commit_tx}], _} = aql:query("COMMIT TRANSACTION;", T1),
    % Back to client 2.
    {ok, [{ok, commit_tx}], _} = aql:query("COMMIT TRANSACTION;", T2).

shared_lock_and_exclusive_lock(_Config) ->
    skip.
    % % Client 1.
    % {ok, _, T1} = aql:query("BEGIN TRANSACTION;"),
    % {ok, [], T1} = aql:query(
    %     "INSERT INTO Employee (Name, Employer) VALUES ('John', 'Company A');",
    %     T1
    % ),
    % % Client 2.
    % {ok, _, T2} = aql:query("BEGIN TRANSACTION;"),
    % {ok, [], T2} = aql:query(
    %     "INSERT INTO Employee (Name, Employer) VALUES ('Bob', 'Company A');",
    %     T2
    % ),
    % % Back to client 1.
    % {ok, [{ok, commit_tx}], _} = aql:query("COMMIT TRANSACTION;", T1),

    % % Client 3 tries to delete company and acquire an exclusive lock, it fails.
    % {ok, _, T3} = aql:query("BEGIN TRANSACTION;"),
    % aql:query("DELETE FROM Company WHERE Name = 'Company A';", T3),

    % % Back to client 2.
    % {ok, [{ok, commit_tx}], _} = aql:query("COMMIT TRANSACTION;", T2).
