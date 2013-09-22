-module(reader).

-export([start_link/0, read/1]).

%% ===================================================================
%% API
%% ===================================================================
start_link() ->
    Path = proplists:get_value(path, application:get_all_env()),
    Pid = proc_lib:spawn_link(?MODULE, read, [Path]),
    {ok, Pid}.

%% ===================================================================
%% Internal functions
%% ===================================================================
stop() ->
    case dist_server:is_finished() of
        true ->
            application:stop(gerlp);
        false ->
            timer:sleep(1000),
            stop()
    end.

read([]) ->
    stop();

read([X|Rest]) when is_list(X) ->
    read_file(X),
    read(Rest).

read_file(X) ->
    {ok, ChunkSize} = application:get_env(chunk_size),
    case file:open(X, [read, raw, {read_ahead, ChunkSize}]) of
        {ok, Dev} ->
            read_chunk(X, Dev, ChunkSize),
            file:close(Dev);
        {error, Reason} ->
            erlang:error(Reason)
    end.

read_chunk(File, Dev, Size) ->
    read_chunk(File, Dev, Size, 1, []).

read_chunk(File, Dev, Size, N, Acc) ->
    case file:read(Dev, Size) of
        {ok, Data} ->
            Ln = string:rchr(Data, $\n),
            case Ln of
                0 ->
                    Merge = lists:flatten([Acc|Data]),
                    read_chunk(File, Dev, Size, N + 1, Merge);
                X ->
                    Merge = lists:flatten([Acc|string:substr(Data, 1, X)]),
                    send_chunk(File, N, Merge),
                    read_chunk(File, Dev, Size, N + 1,
                               string:substr(Data, X + 1))
            end;
        eof ->
            send_chunk(File, N, Acc);
        {error, Reason} ->
            erlang:error(Reason)
    end.

send_chunk(File, N, Data) ->
    dist_server:find(File, N, Data).
