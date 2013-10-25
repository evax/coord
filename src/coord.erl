-module(coord).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(service, {
    name,
    mod,
    opts,
    state,
    first,
    nodes,
    discovery=true
}).

-export([has_service/1, has_service/2]).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% Checks if the given service is handled
%%
%% @spec has_service(atom()) -> boolean()
%% @end
%%--------------------------------------------------------------------
has_service(Name) ->
    case lists:keyfind(Name, 1, application:get_env(coord, services, [])) of
        {Name, _, _, _} -> true;
        _ -> false
    end.

%%--------------------------------------------------------------------
%% @doc
%% Checks if the given service is handled on the given node
%%
%% @spec has_service(atom(), node()) -> boolean()
%% @end
%%--------------------------------------------------------------------
has_service(Name, Node) ->
    case rpc:call(Node, coord, has_service, [Name]) of
        true -> true;
        _ -> false
    end.

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

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
    net_kernel:monitor_nodes(true),
    Nodes = nodes(),
    Services = lists:map(
                 fun({Name, Timeout, Module, Opts}) ->
                    ServiceNodes = lists:foldl(fun(Node, Acc) ->
                                        case has_service(Name, Node) of
                                            true -> [Node|Acc];
                                            _ -> Acc
                                        end
                                    end, [], Nodes),
                    {MState, D} = case ServiceNodes of
                        [] ->
                            {undefined, true};
                        _  ->
                            {Module:init(false, ServiceNodes, Opts), false}
                    end,
                    erlang:start_timer(Timeout, self(), Name),
                    #service{name=Name,mod=Module,opts=Opts,
                             first=ServiceNodes =:= [],
                             nodes=ServiceNodes,
                             state=MState,
                             discovery=D}
        end, application:get_env(coord, services, [])),
    nodefinder:discover(),
    {ok, Services}.

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
handle_info({nodeup, Node}, Services) ->
    Services2 = lists:foldl(fun(#service{name=Name,mod=M,opts=Opts,
                                         nodes=Nodes,
                                         discovery=D,state=State}=S, Acc) ->
        case {has_service(Name, Node), D} of
            {true, true} ->
                MState = M:init(false, [Node], Opts),
                [S#service{first=false,discovery=false,nodes=[Node],state=MState}|Acc];
            {true, false} ->
                MState = M:nodeup(Node, State),
                [S#service{nodes=[Node|Nodes],state=MState}|Acc];
            _ -> Acc
        end
    end, [], Services),
    {noreply, Services2};
handle_info({nodedown, Node}, Services) ->
    Services2 = lists:foldl(fun(#service{mod=M, nodes=Nodes,
                                         state=State}=S, Acc) ->
            case lists:member(Node, Nodes) of
                true ->
                    MState = M:nodedown(Node, State),
                    [S#service{nodes=lists:delete(Node, Nodes),state=MState}|Acc];
                _ -> Acc
        end
    end, [], Services),
    {noreply, Services2};
handle_info({timeout, _Ref, Name}, Services) ->
    Services2 = case lists:keyfind(Name, 2, Services) of
        #service{mod=M,opts=Opts,first=First,nodes=Nodes,discovery=true}=S ->
            MState = M:init(First, Nodes, Opts),
            lists:keyreplace(Name, 2, Services, S#service{discovery=false,
                                                          state=MState});
        _ -> Services
    end,
    {noreply, Services2};
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
