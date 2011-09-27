## So far ...

    $ rebar get-deps
    $ rebar compile
    $ erl -pa deps/rejson/ebin ebin

    1> {ok, S} = jsonspc_query:start_link().
    {ok, <0.33.0>}
    2> {ok, E} = rejson:parse("foo = [1, 2, number *]").
    {ok,{capture,"foo",[1,2,{star,number}]}}
    3> f(Ref), Ref = jsonspc_query:read(S, E, self()).
    #Ref<0.0.0.35>
    4> f(R), receive {result, R, Ref} -> R after 100 -> no end.
    no
    5> jsonspc_query:write(S, [1, 2, 3, 4]).
    ok
    6> f(R), receive {result, R, Ref} -> R after 100 -> no end.
    [{"foo",[1,2,3,4]}]

## What?

This is an experiment in writing a database with tuplespace-like
semantics, using rejson pattern matching (which is described [over
here](https://github.com/squaremo/rejson#readme)).

There are three operations:

 * `write(Value)`
 * `read(Expression)`
 * `take(Expression)`

`read` and `take` return the result of successfully matching the given
expression with a value from those values that have been, or will be,
supplied with `write`. `take` removes the value from the database;
`read` leaves it there. Successive calls to `read` may not return the
same value -- the choice is non-deterministic.
