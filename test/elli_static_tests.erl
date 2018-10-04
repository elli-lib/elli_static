-module(elli_static_tests).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

elli_static_test_() ->
    {setup,
     fun setup/0, fun teardown/1,
     [?_test(readme()),
      ?_test(no_file()),
      ?_test(not_found()),
      ?_test(safe_traversal()),
      ?_test(unsafe_traversal())]}.


readme() ->
    {ok, Response} = httpc:request("http://localhost:3000/elli_static/README.md"),
    {ok, File} = file:read_file("README.md"),
    Expected = binary_to_list(File),
    ?assertEqual([integer_to_list(iolist_size(Expected))],
                 proplists:get_all_values("content-length", element(2, Response))),
    ?assertMatch({_Status, _Headers, Expected}, Response).


no_file() ->
    {ok, Response} = httpc:request("http://localhost:3000/elli_static/no_file"),
    ?assertMatch({{"HTTP/1.1",404,"Not Found"}, _Headers, "Not Found"}, Response).


not_found() ->
    {ok, Response} = httpc:request("http://localhost:3000/not_found"),
    ?assertMatch({{"HTTP/1.1",404,"Not Found"}, _Headers, "Not Found"}, Response).

safe_traversal() ->
    {ok, Response} = httpc:request("http://localhost:3000/elli_static/"
                                   "../elli_static/README.md"),
    {ok, File} = file:read_file("README.md"),
    Expected = binary_to_list(File),
    ?assertEqual([integer_to_list(iolist_size(Expected))],
                 proplists:get_all_values("content-length", element(2, Response))),
    ?assertMatch({_Status, _Headers, Expected}, Response).

unsafe_traversal() ->
    %% compute the relative path to /etc/passwd
    {ok, Cwd} = file:get_cwd(),
    PasswdPath = [".." || _ <- filename:split(Cwd)] ++ ["etc", "passwd"],
    Path = filename:join(PasswdPath),

    {ok, Response} = httpc:request("http://localhost:3000/elli_static/" ++ Path),
    ?assertMatch({{"HTTP/1.1",404,"Not Found"}, _Headers, "Not Found"}, Response).

setup() ->
    {ok, Dir} = file:get_cwd(),
    Args = [{<<"/elli_static">>, {dir, Dir}}],
    Config = [
              {mods, [
                      {elli_middleware_cache, [{elli_static, Args}]},
                      {elli_static, Args}
                     ]
              }
             ],
    {ok, Pid} = elli:start_link([{callback, elli_middleware},
                                 {callback_args, Config},
                                 {port, 3000}]),
    unlink(Pid),
    [Pid].


teardown(Pids) ->
    [elli:stop(Pid) || Pid <- Pids].
