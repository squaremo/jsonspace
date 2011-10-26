-module(jsonspc).

-export([start/0]).

start() ->
    application:start(sockjs),
    application:start(cowboy),
    application:start(jsonspc).
