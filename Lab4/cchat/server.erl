-module(server).
-export([handle/2, initial_state/1]).
-include_lib("./defs.hrl").

%% initial_state/2 and handle/2 are used together with the genserver module,
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
    case lists:keymember(Name, 1, St#server_st.users) of
      false ->
        NewState = St#server_st{users = [{Name, Pid} | St#server_st.users]},
        {reply, ok, NewState};
      true ->
        {reply, {error, user_already_connected}, St}
      end;

%% Used to remove user from server
handle(St, {disconnect, Name, Pid}) ->
    case lists:member({Name, Pid}, St#server_st.users) of
      false ->
        {reply, {error, user_not_connected, "User not connected"}, St};
      true ->
        NewState = St#server_st{users = St#server_st.users -- [{Name, Pid}]},
        {reply, ok, NewState}
    end;

%% Creates a channel, if there is no channel of that provided name then the server spawns one
handle(St, {join, Channel, Name, Pid}) ->
    ChannelAtom = list_to_atom(Channel),
    case lists:member(Channel, St#server_st.channels) of
      false ->
        State = channel:initial_state(Channel),
        F = fun channel:handle/2,
        genserver:start(ChannelAtom, State, F),
        NewState = St#server_st{channels = [Channel | St#server_st.channels]},
        genserver:request(ChannelAtom, {join, Name, Pid}),
        {reply, ok, NewState};
      true ->
        genserver:request(ChannelAtom, {join, Name, Pid}),
        {reply, ok, St}
    end;

%% Handles sending jobs to the clients
handle(St, {send_job, F, Args}) ->
    spawn(client_pool, start_pool, {St#server_st.users, F, Args, self()}),
    {reply, ok, St};

%% This code should not be reached, if it does then an invalid request name has been sent.
handle(St, Request) ->
    {reply, {error, invalid_request, "Server cannot handle request"}, St}.
