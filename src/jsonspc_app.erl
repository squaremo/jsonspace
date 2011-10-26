-module(jsonspc_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    SupOk = jsonspc_sup:start_link(),
    jsonspc_web:start(),
    SupOk.

stop(_State) ->
    ok.
