# AQL

An SQL-like interface for [AntidoteDB](https://github.com/antidotedb/antidote).

## Building

**NOTE:** At the moment, this repository does not work with the upstream version of AntidoteDB, it only works with this version [https://github.com/mrshankly/antidote/tree/aql-strong-consistency-3-patched](https://github.com/mrshankly/antidote/tree/aql-strong-consistency-3-patched). Rebar should fetch the correct version, you should not need to worry about this unless you want to modify the AntidoteDB code that AQL uses.

You will need to install [Erlang/OTP](https://www.erlang.org/) and [rebar3](https://rebar3.org/).
The code was tested with Erlang/OTP 23.2 and rebar3 version 3.14.1.

Once you have erlang and rebar3 installed and working, clone this repository and run:

```shell
$ make
```

This will compile the code and build a release. Run the unit tests along with
the ct tests to make sure that everything is working:

```shell
$ make test ct
```

There a few extra tests available, although they are a bit cumbersome to run. To run the script from a different directory you will need to define the `AQL_HOME` environment variable, which should contain the path to AQL's repository.

```shell
$ cd scripts/tests
$ ./tests_release.sh tests_release.config [NUM_TEST]
```

Where `NUM_TEST` is a number between 1 and 12. See the file [tests_release.config](scripts/tests/tests_release.config) for more information.

### Start the database

To start and run the database do:

```shell
$ ./_build/default/rel/aql/bin/env foreground
```

### Erlang Shell and AQL Shell

To get an erlang shell run `make shell`. If you want an AQL shell to directly execute queries
(the shell is simple and primitive) run `make aqlshell`.

### HTTP server and Protobuf protocol

Clients can communicate with AQL in two ways. Through an HTTP, or TCP + Protobuf.
The default port of the Protobuf interface is 8321, the default port of the HTTP server is 8322.
More information about the HTTP server and Protobuf protocol can be found in the file [docs/http_and_protobuf.md](docs/http_and_protobuf.md).

### Benchmarking (FMKe)

It's possible to benchmark AQL with the [FMKe benchmark](https://github.com/goncalotomas/FMKe).
More information on how to do this is available in the file [docs/fmke_benchmark.md](docs/fmke_benchmark.md). You should also check [FMKe's wiki](https://github.com/goncalotomas/FMKe/wiki) for more information.

---

## Docker

There is an AQL docker image available [here](https://hub.docker.com/r/jbmarques/aql).

Start a local node with the following command:

```shell
$ docker run -d --name aql -p "8321:8321" jbmarques/aql
```

AQL should now be running on port 8321 on localhost. You can connect to the
console of a local node with the following command:

```shell
$ docker exec -it aql /aql/bin/env remote_console
```

You can also use the existing [Dockerfile](Dockerfile) as a guide to build a custom docker image.

---

## Getting started

AQL is an SQL-variant, designed to work with AntidoteDB API.

### API

The AQL API is pretty straightforward. There is a main module called
`aql` with two methods, the `query` and `read_file`.
The `query` method has two headers:
* `query(Query)` receives a query and outputs the result;
* `query(Query, Transaction)` receives a query and a transaction descriptor and outputs the
final results.

Similarly, the `read_file` supports two headers as well:
* `read_file(Filename)` receives a file name, reads and parses a file with AQL statements and returns the result of applying the statements on the database;
* `read_file(Filename, Transaction)` receives a file name and a transaction descriptor reads and parses a file

Therefore, exist two ways of performing a query in AQL. For instance, consider a query to show
all existing tables in the database, While using the Erlang shell mode (activated through
the `make shell` command) this query can me submitted as the following:

```erlang
aql:query("SHOW TABLES").
```

or

```erlang
aql:query("SHOW TABLES;", TxId).
```
This latter example assumes you started a transaction previously (see next section).
While using the native AQL shell (through the `make aqlshell`), this query is submitted on its raw form, like the following:

```
AQL> SHOW TABLES;
```

## AQL Docs

AQL supports multiple SQL-like operations such as:
* Data definition
  * CREATE TABLE
* Data manipulation
  * SELECT
  * INSERT
  * UPDATE
  * DELETE
* Admin
  * SHOW TABLES/INDEX
* Transactions
  * BEGIN
  * COMMIT
  * ABORT

AQL supports a limited set of types:
* VARCHAR - common text data type (similar to SQL's VARCHAR)
* INTEGER/INT - common integer data type (similar to SQL's INTEGER)
* BOOLEAN - common boolean
* COUNTER_INT - integer counter, with bounds
(based on AntidoteDB's
[Bounded Counter](http://www.gsd.inesc-id.pt/~rodrigo/srds15.pdf))

### CREATE TABLE

Creates a new table. If the table already exists the new table will overwrite it
(any concurrent conflicts will be resolved with a *Last Writer Wins* CRP).

```sql
CREATE UPDATE-WINS TABLE Student (
	StudentID INT PRIMARY KEY,
	Name VARCHAR,
	Age INT DEFAULT 18,
	YearsLeft COUNTER_INT CHECK (YearsLeft > 0),
	Passport_id INTEGER FOREIGN KEY UPDATE-WINS REFERENCES Passport(id)
) PARTITION ON (Age);
```

#### Primary keys

The primary key constraint must be specified after the column which is to be
set as the primary key (multiple columns as primary keys are not supported).
Any datatype can be a primary key.

Primary keys only guarantee uniqueness. Although, if two rows with the same
primary key are inserted (either concurrently or not), both insertions will be
merged, and all columns will also be merged according to its datatypes.

#### Check Constraints

AQL also supports constraints on counters (`counter_int`). Assign numeric bounds
to any `COUNTER_INT` column by:

```sql
CHECK (column_name [ < | <= | > | >= ] value)
```

Where `column_name` is the column and `value` is the respective bound.

#### Default Values

You can also define a default value for a record (not allowed in primary keys).
Default values are used when no value is specified for a record.

```sql
column_x data_type DEFAULT value
```

Where `value` is the default value.

#### Foreign Keys

Foreign keys allow users to create custom relations between elements of different
tables. To create a foreign key relation simply add to the column that will be
the foreign key: `FOREIGN KEY [ UPDATE-WINS | DELETE-WINS ] REFERENCES parentTable(parentColumn) [ ON DELETE CASCADE ]`,
where `parentTable` is the parent table name (e.g. `Passport`) and `parentColumn` is
the parent column name (e.g. `id`). All foreign keys must point to columns with a
unique constraint, which is only guaranteed in primary keys.

Additionally you can define a row's behaviour upon a parent deletion through the notation
`ON DELETE CASCADE`, which tells a row to be removed if its parent row is deleted.
The absence of this notation implies that the parent cannot be deleted if one or more rows point to it.

Update-wins (`UPDATE-WINS`) and Delete-wins (`DELETE-WINS`) are conflict resolution policies used
to resolve any referential integrity related conflicts generated by concurrent operations.
Update-wins will revive all rows (deleted) involved in the conflict, while
Delete-wins deletes all involved rows in case of conflict. If none of these policies is specified,
the table assumes strong semantics that preclude parent rows to be deleted concurrently with the
update of child rows.

#### Partitioning

The CREATE TABLE statement allows to partition a table by column, which is most known as horizontal partitioning.
Hence, to partition a table use:

```sql
PARTITION ON (column_name);
```
, which indicates that the table will split its rows by the column `column_name`.

### SELECT

SELECT is the main read operation in AQL (similar to SQL). The operation issues
a read operation in the database engine (AntidoteDB).

```sql
SELECT * FROM Student WHERE StudentID = 20;
```

This operation supports conjunctions (`AND`) and disjunctions (`OR`), and parenthesis to group sub-queries.
A sub-query may be a sequence of one or more comparisons on the form:

```
column_name [ = | <> | < | <= | > | >= ] value
```

### INSERT

Inserts new records in a table. If a value with the primary key already exists it
will be overwritten.

```sql
INSERT INTO (StudentID, Name, Age, YearsLeft, Passport_id) VALUES (10000, 'John', 'Smith', '24', 'ABC');
```

The table columns may be omitted, in which case all columns will be considered on the insertion.
*Note*: string values must always be between single quotes (`'`).

### UPDATE

Updates an already-existent row on the specified table.

```sql
UPDATE Student
SET Age = 25
WHERE StudentID = 10000;
```

Updates all rows in table `Students` where `StudentID` has value 1. The update
sets column `Age` to value `25`. All update operations on columns are based on equalities with different expressions depending on the column's datatype:
* *VARCHAR*/*INTEGER*:
  * `Col = val` sets the column `Col` of type `VARCHAR`/`INTEGER` or `INT` to the value `val` specified (`val` must be a number for the integer datatype).
* *COUNTER_INT*:
  * `Col = Col + val` increments the column `Col` of type `COUNTER_INT` by the value `val` specified.
  * `Col = Col - val` decrements the column `Col` of type `COUNTER_INT` by the value `val` specified.
* *BOOLEAN*:
  * `Col = true` sets the boolean column `Col` to `true`.
  * `Col = false` sets the boolean column `Col` to `false`.
  * In both cases, the boolean value to assign is not enclosed between single quotes.

Unlike the SELECT clause, the WHERE clause on the UPDATE statement can only filter primary keys.

### DELETE

Deletes a set of records from the specified table.

```sql
DELETE FROM Persons Where StudentID = 20525;
```

Just like in an UPDATE operation, the WHERE clause can only filter primary keys.
If the WHERE clause is absent, all the records in the table are deleted.

### TRANSACTION

Just like in SQL, AQL allows to execute a set of queries inside a transaction.

```sql
BEGIN TRANSACTION;
query_1;
query_2;
...
query_n;
[ COMMIT | ROLLBACK ] TRANSACTION;
```

At the end, the transaction can be committed or aborted.
An ongoing transaction must always be terminated first before starting a new one.
