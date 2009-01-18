-module(tut16).
-export([start/0, ping/1, pong/0]).

ping(0) ->
    pong_rname ! finished,
    io:format("ping finished~n", []);
ping(N) ->
    pong_rname ! {ping_message, self()},
    receive
        pong_message ->
            io:format("Ping received pong_message~n", [])
    end,
    ping(N - 1).

pong() ->
    receive
        finished ->
            io:format("Pong finished~n", []);
        {ping_message, Ping_PID} ->
            io:format("Pong received ping_message~n", []),
            Ping_PID ! pong_message,
            pong()
    end.
start() ->
    register(pong_rname, spawn(tut16, pong, [])),
    spawn(tut16, ping, [3]).
