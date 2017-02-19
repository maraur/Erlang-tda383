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
  io:fwrite("I'm alive!!! ~p~n", [St#channel_st.name]),
  {reply, ok, NewState};

% Removes a user from a channel_st
% Server should handle checks
handle(St, {leave, Name, Pid}) ->
  NewState = St#channel_st{users = St#channel_st.users -- [{Name, Pid}]},
  {reply, ok, NewState};

% Handles sending messages to all members of a channel
handle(St, {msg, Name, Pid, Msg}) ->
    % No idea how to do this part
    {reply, {error, not_implemented, "Not implemented"}, St}.
