-module(jsonspc_web).

-export([start/0]).

dispatcher() ->
    [{jsonspace, fun incoming/2}].

incoming(Conn, {recv, Data}) ->
    case binary_to_list(Data) of
        "read " ++ PatternString ->
            case rejson:parse(PatternString) of
                {ok, Pattern} ->
                    spawn(fun() ->
                                  Ref = jsonspc_query:read(jsonspc_query, Pattern, self()),
                                  receive
                                      {result, Bindings, Ref} ->
                                          Conn:send(jsonify(Bindings))
                                  end
                          end);
                _Else ->
                    Conn:close(3000, "Malformed reJSON pattern")
            end;
        "take " ++ PatternString ->
            case rejson:parse(PatternString) of
                {ok, Pattern} ->
                    spawn(fun() ->
                                  Ref = jsonspc_query:take(jsonspc_query, Pattern, self()),
                                  receive
                                      {result, Bindings, Ref} ->
                                          Conn:send(jsonify(Bindings))
                                  end
                          end);
                _Else ->
                    Conn:close(3000, "Malformed reJSON pattern")
            end;
        "write " ++ ValueString ->
            case json:decode(ValueString) of
                {ok, Value} ->
                    jsonspc_query:write(jsonspc_query, Value);
                _Else ->
                    Conn:close(3000, "Malformed JSON value")
            end;
        _Else ->
            Conn:close(3000, "Invalid operation")
    end;
incoming(_Conn, _Else) ->
    ok.

start() ->
    Dispatch = [{'_', [{'_', sockjs_cowboy_handler,
                        {fun handle/1, fun ws_handle/1}}]}],
    cowboy:start_listener(http, 100,
                          cowboy_tcp_transport, [{port, 5975}],
                          cowboy_http_protocol, [{dispatch, Dispatch}]).

handle(Req) ->
    {Path0, Req1} = sockjs_http:path(Req),
    case Path0 of
        "/jsonspace.js" ->
            serve_file(Req1, "jsonspace.js");
        "/" ++ Path ->
            case sockjs_filters:handle_req(Req1, Path, dispatcher()) of
                nomatch ->
                    sockjs_http:reply(404, [], "Not found", Req1);
                Req2 ->
                    Req2
            end
    end.

ws_handle(Req) ->
    {"/" ++ Path, Req1} = sockjs_http:path(Req),
    {Receive, _, _, _} = sockjs_filters:dispatch('GET', Path, dispatcher()),
    {Receive, Req1}.

serve_file(Req, Filename) ->
    {file, ModFile} = code:is_loaded(?MODULE),
    Path = filename:join([filename:dirname(filename:dirname(ModFile)),
                          "priv", "www", Filename]),
    case file:read_file(Path) of
        {ok, Bytes} ->
            sockjs_http:reply(200, [], Bytes, Req);
        {error, _} ->
            sockjs_http:reply(404, [], "Not found", Req)
    end.

jsonify(Bindings) ->
    {ok, Bin} =
        json:encode({[{list_to_binary(Var), Value} || {Var, Value} <- Bindings]}),
    Bin.
