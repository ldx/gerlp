-module(dist_server).

-behaviour(gen_server).

%% API
-export([start_link/0, find/3, match/4, is_finished/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {pool, re, sent, received, next}).

%% ===================================================================
%% API
%% ===================================================================
find(File, N, Data) ->
    gen_server:cast(?MODULE, {find, File, N, Data}).

match(Node, File, N, Matches) ->
    gen_server:cast({?MODULE, Node}, {match, File, N, Matches}).

is_finished() ->
    gen_server:call(?MODULE, {is_finished}, infinity).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

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
    process_flag(trap_exit, true),
    Pool = pool:start(gerlp),
    io:format("started pool ~p~n", [Pool]),
    transfer_code(Pool, dist_server),
    transfer_code(Pool, grep),
    {ok, Pattern} = application:get_env(pattern),
    {ok, RE} = re:compile(Pattern),
    {ok, #state{pool = Pool, re = RE, sent = gb_sets:new(),
                received = dict:new(), next = dict:new()}}.

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
handle_call({is_finished}, _From, State) ->
    io:format("sent set size is ~p~n", [gb_sets:size(State#state.sent)]),
    Reply = case gb_sets:size(State#state.sent) of
        0 -> true;
        _ -> false
    end,
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    {stop, unexpected_call, State}.

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
handle_cast({find, File, N, Data}, State) ->
    Next = case dict:is_key(File, State#state.next) of
        true -> State#state.next;
        false -> dict:store(File, 1, State#state.next)
    end,
    _Pid = pool:pspawn(grep, grep, [node(), File, N, Data, State#state.re]),
    Sent = gb_sets:insert({File, N}, State#state.sent),
    {noreply, State#state{sent = Sent, next = Next}};

handle_cast({match, File, N, Lines}, State) ->
    {Next, Sent, Received} = case dict:fetch(File, State#state.next) of
        N ->
            OldSent = gb_sets:delete({File, N}, State#state.sent),
            print_lines(File, N, Lines),
            maybe_print_next_lines(File, N + 1,
                                   State#state.next,
                                   OldSent,
                                   State#state.received);
        _ ->
            {State#state.next,
             State#state.sent,
             dict:store({File, N}, Lines, State#state.received)}
    end,
    {noreply, State#state{next = Next, sent = Sent, received = Received}};

handle_cast(_Msg, State) ->
    {stop, unexpected_cast, State}.

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
handle_info({'EXIT', _Pid, normal}, State) ->
    {noreply, State};

handle_info(_Info, State) ->
    {stop, unexpected_info, State}.

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
terminate(Reason, _State) ->
    io:format("terminating (~p)~n", [Reason]),
    pool:stop(),
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

%% ===================================================================
%% Internal functions
%% ===================================================================
print_lines(_File, _N, []) ->
    ok;

print_lines(File, N, [Line|Rest]) when is_list(Line) ->
    io:format("~p:~p ~p~n", [File, N, Line]),
    print_lines(File, N, Rest).

maybe_print_next_lines(File, N, Next, Sent, Received) ->
    Key = {File, N},
    case dict:is_key(Key, Received) of
        true ->
            print_lines(File, N, dict:fetch(Key, Received)),
            maybe_print_next_lines(File, N + 1,
                                   Next,
                                   gb_sets:delete(Key, Sent),
                                   dict:erase(Key, Received));
        false ->
            Next1 = dict:store(File, N, Next),
            {Next1, Sent, Received}
    end.

transfer_code([], _Module) ->
    ok;

transfer_code([Node|Rest], Module) ->
    {_Module, Binary, Filename} = code:get_object_code(Module),
    rpc:call(Node, code, load_binary, [Module, Filename, Binary]),
    transfer_code(Rest, Module).
