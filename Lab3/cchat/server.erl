-module(server).
-export([handle/2, initial_state/1]).
-include_lib("./defs.hrl").

%% inititial_state/2 and handle/2 are used togetger with the genserver module,
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

handle(St, {connect, Name}) -> % Do we need to save Pid?
    io:fwrite("User trying to connect: ~p~n", [Name]),
    case lists:member(Name, St#server_st.users) of
    false ->
      NewState = St#server_st{users = [Name | St#server_st.users]},
      {reply, ok, NewState};
    true ->
      {reply, {error, name_taken}, St}
    end;

%% Used to remove user from server
handle(St, {disconnect, Name}) ->
    case lists:member(Name, St#server_st.users) of
      false ->
        {reply, {error, user_not_connected, "User not connected"}, St};
      true ->
        NewState = St#server_st{users = St#server_st.users -- [Name]},
        {reply, ok, NewState}
    end;

%% is this really good? channels needs to be more complex?
%% How do I notify the clients following the channel with this?
handle(St, {join, Channel, Name}) ->
    case lists:member(Channel, St#server_st.channels) of
      true ->
        {reply, ok, St};
      false ->
        NewState = St#server_st{channels = [Channel | St#server_st.channels]},
        {reply, ok, NewState}
    end;

%% Feels like I should need this one although I really don't as the channels
%% doesn't know the clients
handle(St, {leave, Channel}) ->
   {reply, {error, not_implemented, "Not implemented"}, St} ;

handle(St, Request) ->
    io:fwrite("Server received: ~p~n", [Request]),
    Response = "hi!",
    io:fwrite("Server is sending: ~p~n", [Response]),
    {reply, Response, St}.