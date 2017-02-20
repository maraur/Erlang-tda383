-module(channel).
-export([handle/2, initial_state/1]).
-include_lib("./defs.hrl").

% Produce initial state
initial_state(ChannelName) ->
    #channel_st{name = ChannelName}.

%% ---------------------------------------------------------------------------

% Adds a user to a channel
% Since the server ensures that we don't have duplicate names we can ignore
% that here
handle(St, {join, Name, Pid}) ->
  NewState = St#channel_st{users = [{Name, Pid} | St#channel_st.users]},
  io:fwrite("Currently connected users: ~p~n", [NewState#channel_st.users]),
  {reply, ok, NewState};

% Removes a user from a channel_st
% Server/client should handle checks
% I don't think that this is unsafe anyway, worst case is that I try to remove
% a user that doesn't exist
handle(St, {leave, Name, Pid}) ->
  io:fwrite("User trying to leave ~p~n", [Name]),
  NewState = St#channel_st{users = St#channel_st.users -- [{Name, Pid}]},
  io:fwrite("Currently connected users: ~p~n", [NewState#channel_st.users]),
  {reply, ok, NewState};

% Handles sending messages to all members of a channel
handle(St, {msg, Name, Pid, Msg}) ->
  Users = St#channel_st.users -- [{Name, Pid}],
  io:fwrite("Sending to users: ~p~n" ,[Users]),
  lists:foreach(fun(R) ->
    genserver:request(element(2,R), {incoming_msg, St#channel_st.name, Name, Msg}) end,
    Users),
  io:fwrite("Message sent to server: ~p~n" ,[#channel_st.name]),
  {reply, ok, St}.
