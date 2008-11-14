%%%-------------------------------------------------------------------
%%% File    : web_router.erl
%%% Author  : asceth <machinist@asceth.com>
%%% Description : Routes requests from mochiweb to whatever apps
%%%                are hooked into it.
%%%
%%% Created :  7 Sep 2008 by asceth <machinist@asceth.com>
%%%-------------------------------------------------------------------
-module(web_router).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% External API
-export([add/5, add/6]).
-export([run/3, run/4]).
-export([delete/5, delete/6]).

-include("logger.hrl").

-record(state, {web_router}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(WebRouter) ->
  gen_server:start_link({local, WebRouter}, ?MODULE, [WebRouter], []).


%%====================================================================
%% External API
%%====================================================================
%%--------------------------------------------------------------------
add(WebRouter, Hook, Module, Function, Seq) ->
  add(WebRouter, Hook, global, Module, Function, Seq).

add(WebRouter, Hook, Path, Module, Function, Seq) ->
  gen_server:call(WebRouter, {add, Hook, Path, Module, Function, Seq}).

delete(WebRouter, Hook, Module, Function, Seq) ->
  delete(WebRouter, Hook, global, Module, Function, Seq).

delete(WebRouter, Hook, Path, Module, Function, Seq) ->
  gen_server:call(WebRouter, {delete, Hook, Path, Module, Function, Seq}).

run(WebRouter, Hook, Args) ->
  run(WebRouter, Hook, global, Args).

run(WebRouter, Hook, Path, Args) ->
  gen_server:call(WebRouter, {run, Hook, Path, Args}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([WebRouter]) ->
  Table = ets:new(WebRouter, [named_table, public]),
  {ok, #state{web_router=Table}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({add, Hook, Path, Module, Function, Seq}, From, #state{web_router=WebRouter} = State) ->
  spawn(fun() -> internal_add(WebRouter, {Hook, Path, Module, Function, Seq}, From) end),
  {noreply, State};
handle_call({delete, Hook, Path, Module, Function, Seq}, From, #state{web_router=WebRouter} = State) ->
  spawn(fun() -> internal_delete(WebRouter, {Hook, Path, Module, Function, Seq}, From) end),
  {noreply, State};
handle_call({run, Hook, Path, Args}, From, #state{web_router=WebRouter} = State) ->
  spawn(fun() -> internal_run(WebRouter, {Hook, Path, Args}, From) end),
  {noreply, State};
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

internal_add(WebRouter, {Hook, Path, Module, Function, Seq}, From) ->
  Reply = case ets:lookup(WebRouter, {Hook, Path}) of
            [{_, Ls}] ->
              El = {Seq, Module, Function},
              case lists:member(El, Ls) of
                true ->
                  ok;
                false ->
                  NewLs = lists:merge(Ls, [El]),
                  ets:insert(WebRouter, {{Hook, Path}, NewLs}),
                  ok
              end;
            [] ->
              NewLs = [{Seq, Module, Function}],
              ets:insert(WebRouter, {{Hook, Path}, NewLs}),
              ok
          end,
  gen_server:reply(From, Reply).

internal_delete(WebRouter, {Hook, Path, Module, Function, Seq}, From) ->
  Reply = case ets:lookup(WebRouter, {Hook, Path}) of
            [{_, Ls}] ->
              NewLs = lists:delete({Seq, Module, Function}, Ls),
              ets:insert(WebRouter, {{Hook, Path}, NewLs}),
              ok;
            [] ->
              ok
          end,
  gen_server:reply(From, Reply).

internal_run(_WebRouter, {request_error, Path, _Args}, From) ->
  Reply = [{status, Path, headers, [], body, [integer_to_list(Path), <<" error">>]}],
  gen_server:reply(From, Reply);
internal_run(WebRouter, {Hook, Path, Args}, From) ->
  Reply = case ets:lookup(WebRouter, {Hook, Path}) of
            [{_, Ls}] ->
              [run1(Hook, Module, Function, Args) || {_Seq, Module, Function} <- Ls];
            [] ->
              []
          end,
  gen_server:reply(From, Reply).

run1(Hook, Module, Function, Args) ->
  case catch apply(Module, Function, Args) of
    {'EXIT', Reason} ->
      ?ERROR_MSG("~p~nrunning hook: ~p", [Reason, {Hook, Args}]),
      [];
    stop ->
      [];
    ok ->
      [];
    Result ->
      Result
  end.
