%%%-------------------------------------------------------------------
%%% @author Michael Bridgen <mikeb@squaremobius.net>
%%% @copyright (C) 2011, Michael Bridgen
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(jsonspc_query).

-behaviour(gen_server).

%% API
-export([start_link/0, read/3, write/2, take/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, { outstanding, values }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

read(Server, Expression, ReplyTo) ->
    Ref = make_ref(),
    gen_server:cast(Server, {read, Expression, ReplyTo, Ref}),
    Ref.

write(Server, Value) ->
    gen_server:cast(Server, {write, Value}).

take(Server, Expression, ReplyTo) ->
    Ref = make_ref(),
    gen_server:cast(Server, {take, Expression, ReplyTo, Ref}),
    Ref.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{ outstanding = queue:new(),
                 values = queue:new() }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------


%% Read a value without removing it.
handle_cast(Q = {read, Expression, ReplyTo, Ref},
            State = #state{ outstanding = Queries }) ->
    case read_value(Expression, State) of
        none ->
            AddedQuery = queue:in(Q, Queries),
            {noreply, State#state{ outstanding = AddedQuery }};
        {Bindings, NewState} ->
            ReplyTo ! {result, Bindings, Ref},
            {noreply, NewState}
    end;

%% Read a value and remove it.
handle_cast(Q = {take, Expression, ReplyTo, Ref},
            State = #state{ outstanding = Queries }) ->
    case take_value(Expression, State) of
        none ->
            AddedQuery = queue:in(Q, Queries),
            {noreply, State#state{ outstanding = AddedQuery }};
        {Bindings, NewState} ->
            ReplyTo ! {result, Bindings, Ref},
            {noreply, NewState}
    end;

handle_cast({write, Value},
            State = #state{ outstanding = Queries }) ->
    case try_outstanding(Value, Queries) of
        {read, NewOutstanding} ->
            NewState = write_value(Value, State),
            {noreply, NewState#state{ outstanding = NewOutstanding }};
        {taken, NewOutstanding} ->
            {noreply, State#state{ outstanding = NewOutstanding }}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

read_value(Expression, State = #state{ values = Values }) ->
    case read_value1(Expression, Values, queue:new()) of
        none ->
            none;
        {Bindings, NewValues} ->
            {Bindings, State#state{ values = NewValues }}
    end.

take_value(Expression, State = #state{ values = Values }) ->
    case take_value1(Expression, Values, queue:new()) of
        none ->
            none;
        {Bindings, NewValues} ->
            {Bindings, State#state{ values = NewValues }}
    end.

write_value(Value, State = #state{ values = Values }) ->
    State#state{ values = queue:in(Value, Values) }.

%% ------

read_value1(Expression, ToTest, Tested) ->
    case queue:out(ToTest) of
        {empty, _Q} ->
            none;
        {{value, V}, Rest} ->
            case rejson:match(Expression, V) of
                no_match ->
                    read_value1(Expression, Rest, queue:in(V, Tested));
                {ok, Bindings} ->
                    {Bindings, queue:join(Tested, queue:in(V, Rest))}
            end
    end.

take_value1(Expression, ToTest, Tested) ->
    case queue:out(ToTest) of
        {empty, _Q} ->
            none;
        {{value, V}, Rest} ->
            case rejson:match(Expression, V) of
                no_match ->
                    take_value1(Expression, Rest, queue:in(V, Tested));
                {ok, Bindings} ->
                    {Bindings, queue:join(Tested, Rest)}
            end
    end.

try_outstanding(Value, Queries) ->
    try_outstanding1(Value, Queries, queue:new()).

try_outstanding1(Value, ToTry, Tried) ->
    case queue:out(ToTry) of
        {empty, _} ->
            {read, Tried};
        {{value, Query}, Rest} ->
            case Query of
                Q = {read, Expression, ReplyTo, Ref} ->
                    case rejson:match(Expression, Value) of
                        no_match ->
                            try_outstanding1(Value, Rest, queue:in(Q, Tried));
                        {ok, Bindings} ->
                            ReplyTo ! {result, Bindings, Ref},
                            try_outstanding1(Value, Rest, Tried)
                    end;
                Q = {take, Expression, ReplyTo, Ref} ->
                    case rejson:match(Expression, Value) of
                        no_match ->
                            try_outstanding1(Value, Rest, queue:in(Q, Tried));
                        {ok, Bindings} ->
                            ReplyTo ! {result, Bindings, Ref},
                            {taken, queue:join(Tried, Rest)}
                    end
            end
    end.
