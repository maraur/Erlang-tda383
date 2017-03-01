-module(channel).
-export([handle/2, initial_state/1]).
-include_lib("./defs.hrl").

% Produce initial state
initial_state(ChannelName) ->
    #channel_st{name = ChannelName}.

%% ---------------------------------------------------------------------------

%% Adds a user to a channel
%% Server ensures that all users have unique names
handle(St, {join, Name, Pid}) ->
  NewState = St#channel_st{users = [{Name, Pid} | St#channel_st.users]},
  {reply, ok, NewState};

%% Removes a user from a channel_st
%% Client handles the check for whether the user is connected to the channel
handle(St, {leave, Name, Pid}) ->
  NewState = St#channel_st{users = St#channel_st.users -- [{Name, Pid}]},
  {reply, ok, NewState};

%% Handles sending messages to all members of a channel by using the function 'msg_to_user'
handle(St, {msg, Name, Pid, Msg}) ->
  Users = St#channel_st.users -- [{Name, Pid}],
  lists:foreach(fun(R) ->
	msg_to_user(element(2,R), St#channel_st.name, Name, Msg) end, Users),
  {reply, ok, St}.

%% Spawns a process for a message to introduce concurrency
msg_to_user(Pid, Channel, Name, Msg) ->
     spawn(genserver, request, [Pid, {incoming_msg, Channel, Name, Msg}]).
