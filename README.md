## So far ...

    $ rebar get-deps
    $ rebar compile
    $ erl -pa deps/rejson/ebin ebin

    1> {ok, S} = jsonspc_query:start_link().
    {ok,<0.33.0>}
    2> {ok, E} = rejson:parse("[1, 2, foo = number]").
    {ok,[1,2,{capture,"foo",number}]}
    3> jsonspc_query:read(S, E, self()).
    ok
    4> f(R), receive R -> R after 100 -> no end.
    no
    5> jsonspc_query:write(S, [1, 2, 3]).
    ok
    6> f(R), receive R -> R after 100 -> no end.
    {result,[{"foo",3}]}

## What?

This is an experiment in writing a database with tuplespace-like
semantics, using rejson pattern matching (which is described [over
here](https://github.com/squaremo/rejson#readme)).

There are three operations:

 * `write(Server, Value)`
 * `read(Server, Expression)`
 * `take(Server, Expression)`

`read` and `take` return the result of successfully matching the given
expression with a value from those values that have been, or will be,
supplied with `write`. `take` removes the value from the database;
`read` leaves it there. Successive calls to `read` may not return the
same value -- the choice is non-deterministic.
