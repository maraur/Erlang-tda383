-module(client_pool).
-export().
-include_lib("./defs.hrl").

%% Should we have a single client_pool that keeps jobs in the State or 
%% should we spawn a new client_pool in serer for each job?


% Produce initial state
start_pool(Clients, F, Tasks, Server) ->
    #pool_st{idles = Clients, tasks = Tasks, Server = server},
    %%client_tasks = assign_tasks(St#pool_st.idles, tasks), <- skip this if we don't absolutely need it
    loop(St, F, 0). %% TODO fix this shit

%% ---------------------------------------------------------------------------

%% Not needed/used?
handle_work(something) -> 
	assign_tasks(something),
	send_tasks(something),
	recieve_tasks(something).

%% Function for assigning tasks to users
%% Not needed/used?
assign_tasks([], _) -> [] ;
assign_tasks(Users, Tasks) ->
  [  {lists:nth(((N-1) rem length(Users)) + 1, Users), Task}
  || {N,Task} <- lists:zip(lists:seq(1,length(Tasks)), Tasks) ].

%% Maybe this is bad, should fix
send_tasks ([], Id) = Id;
send_tasks ([H|T], Id) = genserver:request(something), send_tasks(T, (Id + 1)).


%%add_client()


%%remove_client()


loop(St, F, Ref) ->
   case {St#pool_st.tasks, St#pool_st.idle, St#pool_st.busy} of
	{[],_,[]} ->
		%% use keysort to get elements right order
		%% make list as result
		%% send result to server
	{[],_,_} ->
		%% receive something
		%% client now idle
		%% append to result
		%% loop(stuff)
		loop(NewState, F, Ref);
	{_,[],_} ->
		%% receive something
		%% client now idle
		%% append to result
		loop(NewState, F, Ref);
	_ ->
		%% work_to_client(),
		%% client now busy
		%% loop(stuff)
		loop(NewState, F, (Ref + 1));
   end.

