-module(elli_static_utils).

-export([safe_relative_path/1]).

%% @doc Backport of `filename:safe_relative_path/1' from 19.3. This code was
%% lifted from:
%% https://github.com/erlang/otp/blob/master/lib/stdlib/src/filename.erl#L811
-spec safe_relative_path(Filename) -> 'unsafe' | SafeFilename when
      Filename :: file:name_all(),
      SafeFilename :: file:name_all().

safe_relative_path(Path) ->
    case filename:pathtype(Path) of
        relative ->
            Cs0 = filename:split(Path),
            safe_relative_path_1(Cs0, []);
        _ ->
            unsafe
    end.

safe_relative_path_1(["."|T], Acc) ->
    safe_relative_path_1(T, Acc);
safe_relative_path_1([<<".">>|T], Acc) ->
    safe_relative_path_1(T, Acc);
safe_relative_path_1([".."|T], Acc) ->
    climb(T, Acc);
safe_relative_path_1([<<"..">>|T], Acc) ->
    climb(T, Acc);
safe_relative_path_1([H|T], Acc) ->
    safe_relative_path_1(T, [H|Acc]);
safe_relative_path_1([], []) ->
    [];
safe_relative_path_1([], Acc) ->
    filename:join(lists:reverse(Acc)).

climb(_, []) ->
    unsafe;
climb(T, [_|Acc]) ->
    safe_relative_path_1(T, Acc).
