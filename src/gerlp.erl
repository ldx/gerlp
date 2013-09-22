%% This is used as the entry point for escript.

-module(gerlp).

%% API
-export([main/1]).

-define(PROG, atom_to_list(?MODULE)).

%% ===================================================================
%% API functions
%% ===================================================================
main(Args) ->
    OptSpecList = [{cookie, $c, "cookie", atom, "Set Erlang cookie."},
                   {lname, $n, "name", atom, "Set Erlang node name."},
                   {sname, $s, "sname", atom, "Set Erlang node short name."},
                   {chunk_size, $s, "chunk_size", {integer, 32768},
                    "Set chunk size for reading and processing."},
                   {verbose, $v, "verbose", integer, "Verbosity level."},
                   {help, $h, "help", undefined, "Show usage info."}],
    {ok, {Props, Leftover}} = getopt:parse(OptSpecList, Args),
    case proplists:get_value(help, Props) of
        undefined ->
            ok;
        _ ->
            getopt:usage(OptSpecList, ?PROG),
            halt(0)
    end,
    [Pattern|Path] = Leftover,
    Props1 = lists:concat([Props, [{pattern, Pattern}], [{path, Path}]]),
    start(Props1).

%% ===================================================================
%% Internal functions
%% ===================================================================
set_env(_App, []) ->
    ok;

set_env(App, [{Key, Val}|Rest]) ->
    application:set_env(App, Key, Val),
    set_env(App, Rest).

loop() ->
    Pid = erlang:whereis(gerlp_sup),
    Ref = erlang:monitor(process, Pid),
    receive
        {'DOWN', Ref, process, Pid, shutdown} ->
            halt(0);
        {'DOWN', Ref, process, Pid, _Reason} ->
            ok;
        _ ->
            ok
    end,
    loop().

start(Props) ->
    io:format("starting with ~p~n", [Props]),
    case proplists:get_value(lname, Props) of
        undefined ->
            ok;
        LName ->
            {ok, _} = net_kernel:start([LName, longnames])
    end,
    case proplists:get_value(sname, Props) of
        undefined ->
            ok;
        SName ->
            {ok, _} = net_kernel:start([SName, shortnames])
    end,
    case proplists:get_value(cookie, Props) of
        undefined ->
            ok;
        Cookie ->
            true = erlang:set_cookie(node(), Cookie)
    end,
    ok = application:load(gerlp),
    ok = set_env(gerlp, Props),
    ok = application:start(gerlp),
    loop().
