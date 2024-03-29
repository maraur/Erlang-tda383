% Top level module
-module(cchat).
-export([server/0,client/0,start/0,start2/0,send_job/3]).
-include_lib("./defs.hrl").

%% Start a server
server() ->
    Server = "shire",
    genserver:start(list_to_atom(Server), server:initial_state(Server), fun server:handle/2).

%% Start a client GUI
client() ->
    gui:start().

%% Start local server and one client
start() ->
    server(),
    client().

%% Start local server and two clients
start2() ->
    server(),
    client(),
    client().

send_job(Server, F, Args) ->
    io:fwrite("cchat got it ~n"), %% TODO REMOVE!!!! only for testing
    %%genserver:request(list_to_atom(Server), {send_job, F, Args, self()}, 10000). %% spawn?
    spawn(genserver, request, [list_to_atom(Server), {send_job, F, Args, self()}]),
    receive
      {results, Result} ->
        Result
    end.
