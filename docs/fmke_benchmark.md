# FMKe benchmark

This document provides an outline of the steps you need to take to run the FMKe benchmark with AQL as the underlying database.

It doesn't really matter which database you are benchmarking, the way you run FMKe is basically the same for all of them. Read the [FMKe wiki](https://github.com/goncalotomas/FMKe/wiki) to know more.

## Requirements

To run the FMKe benchmark you will need to have installed the following:

* Erlang/OTP (tested with version 23.2)
* [rebar3](https://www.rebar3.org/) (tested with version 3.14.1)
* R programming language (OPTIONAL: only required if you wish to generate plots of the results)

## Run the benchmark

FMKe is split into 3 components: an application server, populator and client. You will need all 3 to run the benchmark. You can distribute the different FMKe components through different machines, or even run multiple FMKe clients at the same time.

1. Deploy AQL however you want, e.g. a single instance, single cluster, multiple data centers, etc. The way you create AQL clusters is exactly the same as the way you create AntidoteDB clusters. The simplest way is by using `docker-compose`. Refer to the AntidoteDB documentation to learn how to do this [[1]](https://antidotedb.gitbook.io/documentation/quickstart#create-a-cluster) [[2]](https://github.com/AntidoteDB/docker-antidote/tree/master/compose-files) [[3]](https://github.com/AntidoteDB/antidote-connect).

2. **FMKe application server**: Clone the repository from [https://github.com/goncalotomas/FMKe](https://github.com/goncalotomas/FMKe) and modify the file `config/fmke.config` accordingly, the `target_database` should be `aql`.

3. **FMKe application server**: It's possible that the AQL client that FMKe is using is outdated. You can try to run the benchmark without updating the AQL client, but if you encounter any errors, try to upgrade the AQL client. Inside the FMKe application server directory, modify the `rebar.conf` file, inside the `deps` section, change `aqlc` version to the latest. Once that is done, do the following:

    ```shell
    $ rebar3 unlock aqlc
    $ rebar3 upgrade aqlc
    $ rebar3 lock aqlc
    ```

    If there was an update, the AQL client should now be updated.

4. **FMKe application server**: Run `rebar3 shell --name fmke@127.0.0.1`, this will start the FMKe application server.

5. **Database schema**: Before populating the database you will have to create the database tables and indexes. There two versions of the schema, one without foreign keys, `build_schema.aql`, and one with foreign keys, `build_schema_fk.aql`. The last one uses the referential integrity mechanism of AQL. Both files should be inside the `priv` folder of the FMKe application server repository.

    The way you do create the schema depends on how you started the database. If you are **not** using docker (for example, started AQL with the `env foreground` command), you can use the `env` script to read a schema file. `cd` into the AQL directory and use the following command to read an AQL file:

    ```shell
    $ ./_build/default/rel/aql/bin/env eval 'aql:read_file("/full/path/to/FMKe/priv/build_schema.aql").'
    ```

    An alternative is to use the same `env` script to connect with a remote console and run the erlang command (`aql:read_file("...").`) directly.

    If you are using docker, you will have to somehow place that schema file inside the docker container, for example by building a docker image with the file inside, or with the `-v, --volume` option:

    ```shell
    $ docker run -d --name aql -p 8321:8321 -v /path/to/FMKe/priv:/aql/priv jbmarques/aql
    ```

    And then use the same `env` script to read the schema file:

    ```shell
    $ docker exec -i aql /aql/bin/env eval 'aql:read_file("/aql/priv/build_schema.aql").'
    ```

6. **FMKe populator**: Clone the repository from [https://github.com/goncalotomas/fmke_populator](https://github.com/goncalotomas/fmke_populator) and compile the populator with `rebar3 escriptize`.

7. **FMKe populator**: I found that it's best to populate the database in two steps (the `-p` option specifies the number of processes, in the first step of the populator you can change that number however you see fit, in the second step, that number should be 1, if not you can encounter errors):

    ```shell
    $ ./_build/default/bin/fmke_populator --noprescriptions -p 4 -r 1 fmke@127.0.0.1
    $ ./_build/default/bin/fmke_populator --onlyprescriptions -p 1 -r 1 fmke@127.0.0.1
    ```

    There are multiple options available, run `./_build/default/bin/fmke_populator` to see the usage message.

8. **FMKe client**: Clone the repository from [https://github.com/goncalotomas/fmke_client](https://github.com/goncalotomas/fmke_client) and compile the client with `rebar3 escriptize`.

9. **FMKe client**: Create a benchmark configuration file, use the file `examples/fmke_client.config` as an example.

10. **FMKe client**: Start the benchmark with `./_build/default/bin/lasp_bench your_benchmark_config_file.config`. The FMKe client will now generate load and measure the performance of the database. The client has multiple options, to see the usage message run `./_build/default/bin/lasp_bench --help`.

11. **FMKe client**: Once the benchmark is done, the results should be in the directory `tests` (unless you changed the results directory). If you have the R programming language installed you can generate plots by running the following command:

    ```shell
    $ Rscript priv/summary.r -i tests/current
    ```

    A file named `tests/current/summary.png` should be generated.
