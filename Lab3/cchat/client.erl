-module(client).
-export([handle/2, initial_state/2]).
-include_lib("./defs.hrl").

%% inititial_state/2 and handle/2 are used togetger with the genserver module,
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

%% Connect to server
handle(St, {connect, Server}) ->
    % Maybe it's better to get the atom of server right away?
    case St#client_st.server of
    undefined ->
      ServerAtom = list_to_atom(Server),
      case genserver:request(ServerAtom, {connect, St#client_st.nick, self()}) of
      ok ->
	         NewState = St#client_st{server = ServerAtom},
	           {reply, ok, NewState} ;
      {error, name_taken} ->
	         {reply, {error, connection_error, "Nick already taken"}, St}
      end;
    _ -> % is this robust enough?
      {reply, {error, user_already_connected, "you are already connected to a server"}, St}
  end;

%% Disconnect from server
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


% Join channel
handle(St, {join, Channel}) ->
  case St#client_st.server of
    undefined ->
        {reply, {error, user_not_connected, "Not connected to server"}, St};
    _ -> % is this robust enough?
      case lists:member(Channel, St#client_st.channels) of
        true ->
            {reply,{error, user_already_joined, "Already a member of channel"}, St};
        false ->
            genserver:request(St#client_st.server, {join, Channel}),
            NewState = St#client_st{channels = [Channel | St#client_st.channels]},
            {reply, ok, NewState}
      end
    end;

%% Leave channel
handle(St, {leave, Channel}) ->
  case St#client_st.server of
    undefined ->
        {reply, {error, user_not_connected, "Not connected to server"}, St};
    _ -> % is this robust enough?
      case lists:member(Channel, St#client_st.channels) of
        false ->
            {reply, {error, user_not_joined, "Not a member of the channel"}, St};
        true ->
              genserver:request(St#client_st.server, {leave, Channel}),
              NewState = St#client_st{channels = St#client_st.channels -- [Channel]},
              {reply, ok, NewState}
      end
    end;

% Sending messages
handle(St, {msg_from_GUI, Channel, Msg}) ->
    genserver:request(St#client_st.server, {msg_from_GUI, Channel, Msg}),
    {reply, ok, St};

%% Get current nick
handle(St, whoami) ->
    {reply, St#client_st.nick, St} ;

%% Change nick
%% Only accepts lower case for Nick otherwise it doesn't find the command
handle(St, {nick, Nick}) ->
    case St#client_st.server of
	undefined ->
	    NewState = St#client_st{nick = Nick},
	    {reply, ok, NewState} ;
	_ -> % is this robust enough?
	     {reply, {error, user_already_connected, "You are connected to a server"}, St}
    end;

%% Incoming message
handle(St = #client_st { gui = GUIName }, {incoming_msg, Channel, Name, Msg}) ->
    gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name++"> "++Msg}),
    {reply, ok, St}.
