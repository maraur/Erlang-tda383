-module(client_pool).
-export([start_pool/4]).
-include_lib("./defs.hrl").

%% Should we have a single client_pool that keeps jobs in the State or
%% should we spawn a new client_pool in serer for each job?


% Produce initial state
start_pool(Clients, F, Tasks, Server) ->
    St = #pool_st{idle = Clients, tasks = Tasks, server = Server},
    loop(St, F, 0, []). %% TODO fix this shit

%% ---------------------------------------------------------------------------

%%add_client()

%%remove_client()

%loop() ->
%  io:fwrite("yay ~n").

loop(St, F, Ref, Result) ->
   case {St#pool_st.tasks, St#pool_st.idle, St#pool_st.busy} of
	   {[], _, []} ->
       SortedResult = lists:keysort(1, Result),
       FinalResult = [element(2,X) || X <- SortedResult],
io:fwrite("Final result ended up being: ~p~n", [{FinalResult}]); %% TODO remove

		%% use keysort to get elements right order
		%% make list as result
		%% send result to server
	   {[], _, _} ->
		%% receive something
		%% client now idle
		%% append to result
io:fwrite("Waiting to receive ~n"),
       receive
         {Pid, TaskRef, Val} ->
io:fwrite("Client managed to return something: ~p ~n", [{Pid, TaskRef, Val}]), %%TODO remove
             User = lists:keyfind(Pid, 2, St#pool_st.busy),
             NewResult = [{TaskRef, Val} | Result],
             NewState = St#pool_st{idle = St#pool_st.idle ++ [User], busy = St#pool_st.busy -- [User]},
             loop(NewState, F, Ref, NewResult);
         _ ->
             io:fwrite("Client returned something wrong ~n")
           end;
		%  loop(NewState, F, Ref);
	   {_, [], _} ->
io:fwrite("Waiting to receive ~n"),
        receive
          {Pid, TaskRef, Val} ->
io:fwrite("Client managed to return something: ~p ~n", [{Pid, TaskRef, Val}]), %%TODO remove
              User = lists:keyfind(Pid, 2, St#pool_st.busy),
              NewResult = [{TaskRef, Val} | Result],
              NewState = St#pool_st{idle = St#pool_st.idle ++ [User], busy = St#pool_st.busy -- [User]},
              loop(NewState, F, Ref, NewResult);
          _ ->
              io:fwrite("Client returned something wrong ~n")
            end;
		%% client now idle
		%% append to result
	   _ ->
		%% work_to_client(),
		%% client now busy
        User = hd(St#pool_st.idle),
        Task = hd(St#pool_st.tasks),
io:fwrite("trying to spawn server for ~p~n", [{User}]),
        spawn(genserver, request, [element(2, User), {handle_job, F, Task, self(), Ref}]),
        NewState = St#pool_st{idle = St#pool_st.idle -- [User], busy = St#pool_st.busy ++ [User],
                              tasks = St#pool_st.tasks -- [Task]},
io:fwrite("yay4 ~p~n", [{St#pool_st.tasks, St#pool_st.idle, St#pool_st.busy}]), %% TODO remove
		    loop(NewState, F, (Ref + 1), Result)
   end.


%% Function for assigning tasks to users
%% Not needed/used?
%assign_tasks([], _) -> [];
%assign_tasks(Users, Tasks) ->
%   {lists:nth(((N-1) rem length(Users)) + 1, Users), Task}
 %|| {N,Task} <- lists:zip(lists:seq(1,length(Tasks)), Tasks) ].
