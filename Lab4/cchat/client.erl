-module(client).
-export([handle/2, initial_state/2]).
-include_lib("./defs.hrl").

%% initial_state/2 and handle/2 are used togetger with the genserver module,
%% explained in the lecture about Generic server.

%% Produce initial state
initial_state(Nick, GUIName) ->
    #client_st {nick = Nick, gui = GUIName }.

%% ---------------------------------------------------------------------------

%% handle/2 handles each kind of request from GUI

%% All requests are processed by handle/2 receiving the request data (and the
%% current state), performing the needed actions, and returning a tuple
%% {reply, Reply, NewState}, where Reply is the reply to be sent to the
%% requesting process and NewState is the new state of the client.

%% Connect to server if not already connected and the server exists
handle(St, {connect, Server}) ->
  case St#client_st.server of
  undefined ->
    ServerAtom = list_to_atom(Server),
    case whereis(ServerAtom) of
        undefined ->
          {reply, {error, server_not_reached, "Server could not be reached"}, St};
        _ ->
          case catch genserver:request(ServerAtom, {connect, St#client_st.nick, self()}) of
            ok ->
              NewState = St#client_st{server = ServerAtom},
              {reply, ok, NewState} ;
            {error, user_already_connected} ->
              {reply, {error, nick_taken, "Nick already taken"}, St};
            _ ->
              {reply, {error, server_not_reached, "Server could not be reached"}, St}
            end
       end;
  _ ->
    {reply, {error, user_already_connected, "you are already connected to a server"}, St}
  end;

%% Disconnect from server if the user is connected to the server
handle(St, disconnect) ->
    case St#client_st.server of
      undefined ->
	        {reply, {error, user_not_connected, "Not connected to server"}, St};
      _ ->
	       case St#client_st.channels of
	          [] ->
	            genserver:request(St#client_st.server, {disconnect, St#client_st.nick, self()}),
	            NewState = St#client_st{server = undefined},
	            {reply, ok, NewState} ;
	          _ ->
	             {reply, {error, leave_channels_first, "Leave channels before disconnecting"}, St}
	         end
    end;


%% Join channel
%% Uses the server to join the channel, this is the only time that the client asks the server
%% about something related to the channels
handle(St, {join, Channel}) ->
  case St#client_st.server of
    undefined ->
        {reply, {error, user_not_connected, "Not connected to server"}, St};
    _ ->
      case lists:member(Channel, St#client_st.channels) of
        true ->
            {reply,{error, user_already_joined, "Already a member of channel"}, St};
        false ->
            genserver:request(St#client_st.server, {join, Channel, St#client_st.nick, self()}),
            NewState = St#client_st{channels = [Channel | St#client_st.channels]},
            {reply, ok, NewState}
      end
    end;

%% Leave channel
%% Ignores the server as the server does not keep a record of which users are connected to what
%% channel.
handle(St, {leave, Channel}) ->
  case St#client_st.server of
    undefined ->
        {reply, {error, user_not_connected, "Not connected to server"}, St};
    _ ->
      case lists:member(Channel, St#client_st.channels) of
        false ->
            {reply, {error, user_not_joined, "Not a member of the channel"}, St};
        true ->
              ChannelAtom = list_to_atom(Channel),
              genserver:request(ChannelAtom, {leave, St#client_st.nick, self()}),
              NewState = St#client_st{channels = St#client_st.channels -- [Channel]},
              {reply, ok, NewState}
      end
    end;

%% Sending messages
%% Sends the message directly to the channels, skipping the server.
handle(St, {msg_from_GUI, Channel, Msg}) ->
  case lists:member(Channel,St#client_st.channels) of
    true ->
      ChannelAtom = list_to_atom(Channel),
      genserver:request(ChannelAtom, {msg, St#client_st.nick, self(), Msg}),
      {reply, ok, St};
    false ->
      {reply, {error, user_not_joined, "You are not connected to that channel"},St}
    end;

%% Get current nick
handle(St, whoami) ->
    {reply, St#client_st.nick, St} ;

%% Change nick of client
handle(St, {nick, Nick}) ->
    case St#client_st.server of
	undefined ->
	    NewState = St#client_st{nick = Nick},
	    {reply, ok, NewState};
	_ ->
	     {reply, {error, user_already_connected, "You are connected to a server"}, St}
    end;

%% Completes job given by server and returns it
handle(St, {handle_job, F, Args, Pid, Ref}) ->
   Val = F(Args),
   io:fwrite("Client got it and calculated! ~p ~n" , [{St#client_st.nick, Val}]),
   Pid ! {self(), Ref, Val},
   {reply, ok, St};

%% Incoming message
handle(St = #client_st { gui = GUIName }, {incoming_msg, Channel, Name, Msg}) ->
    gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name++"> "++Msg}),
    {reply, ok, St}.
