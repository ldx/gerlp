-module(grep).

-export([grep/5]).

%% ===================================================================
%% API
%% ===================================================================
grep(Node, File, N, Data, RE) ->
    Lines = string:tokens(Data, "\n"),
    Matches = grep_lines(Lines, RE),
    dist_server:match(Node, File, N, Matches).

%% ===================================================================
%% Internal functions
%% ===================================================================
grep_lines([], _RE) ->
    [];

grep_lines([Line|Rest], RE) when is_list(Line) ->
    lists:foldl(fun(Ln, Matches) ->
                case is_matching(Ln, RE) of
                    true -> [Ln|Matches];
                    false -> Matches
                end end,
                [], [Line|Rest]).

is_matching(Line, RE) ->
    case re:run(Line, RE) of
        nomatch -> false;
        _ -> true
    end.
