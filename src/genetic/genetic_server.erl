-module(genetic_server).
-behavior(gen_server).

-record(state, {
								n,
								worker_sup_pid
							 }).


-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-compile([export_all]).



start_link(ParentPid) ->
	gen_server:start_link(?MODULE, ParentPid, []).

init(ParentPid) ->
	io:format("genetic SERVER: started~n"),
	gen_server:cast(self(), {init, ParentPid}),
	N = 2,
	{ok, #state{n=N}}.


handle_call(_E, _From, State) ->
	{noreply, State}.

handle_cast({init, ParentPid}, State) ->
	Name = genetic_worker_sup,
	ChildSpec = {Name, {Name, start_link, [self()]}, permanent, 10000, supervisor, [Name]},
	{ok, WorkerSupPid} = supervisor:start_child(ParentPid, ChildSpec),
	gen_server:cast(self(), create_first_gen),
	{noreply, State#state{worker_sup_pid=WorkerSupPid}};
handle_cast(success, State) ->
	io:format("received success~n"),
	{noreply, State#state{}};
handle_cast(create_first_gen, State) ->
	{noreply, State#state{}};
handle_cast(Msg, State) ->
	io:format("genetic SERVER: received unknown cast: ~p~n", [Msg]),
	{noreply, State}.



handle_info(Msg, State) ->
	io:format("genetic SERVER: received unknown message: ~p~n", [Msg]),
	{noreply, State}.


code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(_Reason, _State) ->
	ok.
