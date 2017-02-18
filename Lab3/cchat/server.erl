-module(server).
-export([handle/2, initial_state/1]).
-include_lib("./defs.hrl").

%% initial_state/2 and handle/2 are used togetger with the genserver module,
%% explained in the lecture about Generic server.

% Produce initial state
initial_state(ServerName) ->
    #server_st{name = ServerName}.

%% ---------------------------------------------------------------------------

%% handle/2 handles requests from clients

%% All requests are processed by handle/2 receiving the request data (and the
%% current state), performing the needed actions, and returning a tuple
%% {reply, Reply, NewState}, where Reply is the reply to be sent to the client
%% and NewState is the new state of the server.

handle(St, {connect, Name, Pid}) ->
    io:fwrite("User trying to connect: ~p~n", [{Name, Pid}]),
    case lists:keymember(Name, 1, St#server_st.users) of
    false ->
      NewState = St#server_st{users = [{Name, Pid} | St#server_st.users]},
      {reply, ok, NewState};
    true ->
      {reply, {error, name_taken}, St}
    end;

%% Used to remove user from server
handle(St, {disconnect, Name, Pid}) ->
  %something genserver:start() I guess?s
    case lists:member({Name, Pid}, St#server_st.users) of
      false ->
        {reply, {error, user_not_connected, "User not connected"}, St};
      true ->
        NewState = St#server_st{users = St#server_st.users -- [{Name, Pid}]},
        {reply, ok, NewState}
    end;

%% TODO Needs to be completely redone so that it spawns a channel
handle(St, {join, Channel, Name, Pid}) ->
    case lists:member(Channel, St#server_st.channels) of
      true ->
        {reply, ok, St};
      false ->
        ChannelAtom = list_to_atom(St#server_st.name ++ Channel),
        {reply, ok, NewState}
    end;
    genserver:request()

% TODO need to be implemented, doesn't pass anything to channel
handle(St, {msg_from_GUI, Chatroom, String}) ->
  io:fwrite("User trying to write: ~p~n", [{String, Chatroom}]),
  {reply, ok, St};


%% TODO implement this one now that channel is a process
handle(St, {leave, Name, Pid, Channel}) ->
  io:fwrite("User trying to leave channel: ~p~n", [{Channel}]),
   {reply, {error, not_implemented, "Not implemented"}, St} ;

handle(St, Request) ->
    {reply, {error, invalid_request, "Server cannot handle request"}, St} .
