% This record defines the structure of the client process.
% Add whatever other fields you need.
% It contains the following fields:
%   nick: name of the client.
%   gui: the name (or Pid) of the GUI process.
%   server: atom of the server to which the client is connected
%   channels: list of channels to which the client is connected
-record(client_st, {nick, gui, server, channels = []}).

% This record defines the structure of the server process.
% Keeps track of it's own name as well as the connected users
% and channels existing on the server.
-record(server_st, {name, users = [], channels = []}).

% This record defines the structure of the channel process.
% Keeps track of it's own name as well as the connected users.
-record(channel_st, {name, users = []}).
gitu
