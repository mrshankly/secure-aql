# HTTP API

AQL provides a simple HTTP API that clients can use to interface with the database.

Start AQL with the following command:

```shell
$ ./_build/default/rel/aql/bin/env foreground
```

Or by using the available docker image:

```shell
$ docker run -d --name aql -p "8322:8322" jbmarques/aql
```

Either way, AQL should now be running on localhost and an HTTP server should be listening on port 8322. To change the port, modify the [config/sys.config.src](../config/sys.config.src) file with the configuration parameter `aql_http_port` for the `aql` application. Refer to the [erlang documentation](https://erlang.org/doc/man/config.html) for more information.

To issue queries, perform a **POST** request to `http://127.0.0.1:8322/aql`, with the parameter `query` that should contain the string of the query that you want to execute. For example:

```shell
$ curl -X POST --data-urlencode 'query=SELECT * FROM Artist;' http://127.0.0.1:8322/aql
```

```json
[
    [
        { "Name": "Sam", "Age": 22, "Country": "ENG" },
        { "Name": "Rob", "Age": 25, "Country": "ITA" },
        { "Name": "Ken", "Age": 33, "Country": "AUS" },
        { "Name": "Ann", "Age": 17, "Country": "USA" },
        { "Name": "Jon", "Age": 40, "Country": "USA" }
    ]
]
```

You can issue multiple queries in a single request. If successful, you should get back a JSON response, a list with as many elements as the number of requests. For example:

```shell
$ curl -X POST --data-urlencode 'query=SELECT * FROM Artist; SELECT * FROM Album;' http://127.0.0.1:8322/aql
```

```json
[
    [
        { "Name": "Sam", "Age": 22, "Country": "ENG" },
        { "Name": "Rob", "Age": 25, "Country": "ITA" },
        { "Name": "Ken", "Age": 33, "Country": "AUS" },
        { "Name": "Ann", "Age": 17, "Country": "USA" },
        { "Name": "Jon", "Age": 40, "Country": "USA" }
    ],
    [
        { "Title": "A0", "Art": "Sam", "Year": 2016 },
        { "Title": "A1", "Art": "Sam", "Year": 2008 },
        { "Title": "A3", "Art": "Jon", "Year": 1998 },
        { "Title": "A6", "Art": "Ken", "Year": 2006 },
        { "Title": "A4", "Art": "Jon", "Year": 2005 },
        { "Title": "A5", "Art": "Jon", "Year": 2001 },
        { "Title": "A2", "Art": "Rob", "Year": 2012 }
    ]
]
```

# Protobuf interface

If you prefer, instead of the HTTP API, you can use the Protobuf interface. By default it uses port 8321, if you wish to change it, modify the [config/sys.config.src](../config/sys.config.src) file with the configuration parameter `aql_pb_port` for the `aql` application. Refer to the [erlang documentation](https://erlang.org/doc/man/config.html) for more information.

The Protobuf message types are defined in the file [proto/aql.proto](../proto/aql.proto).

To send a request to the server use the `Request` message in the following way:

```
Request {
    type = QUERY,
    query = "select * from FooTable;"
}
```

You should receive a message of the type `Response`. If there was an error, the field `query_error` should have a description of what happened. If successful, the field `query` should contain the query result in JSON format, with the same format as it was explained in the HTTP section above.
