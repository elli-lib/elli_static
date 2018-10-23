-module(elli_static_utils_tests).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

elli_static_utils_test_() ->
     [?_test(safe_relative_path())].

safe_relative_path() ->
    %% lists
    ?assertEqual("a", elli_static_utils:safe_relative_path("a")),
    ?assertEqual("a/b", elli_static_utils:safe_relative_path("a/b")),
    ?assertEqual("a/b", elli_static_utils:safe_relative_path("a/./b")),
    ?assertEqual("a/b", elli_static_utils:safe_relative_path("a/./b/.")),
    ?assertEqual("", elli_static_utils:safe_relative_path("a/..")),
    ?assertEqual("", elli_static_utils:safe_relative_path("a/./..")),
    ?assertEqual("", elli_static_utils:safe_relative_path("a/../.")),
    ?assertEqual("a", elli_static_utils:safe_relative_path("a/b/..")),
    ?assertEqual("a", elli_static_utils:safe_relative_path("a/../a")),
    ?assertEqual("a", elli_static_utils:safe_relative_path("a/../a/../a")),
    ?assertEqual("a/b/c", elli_static_utils:safe_relative_path("a/../a/b/c")),
    ?assertEqual(unsafe, elli_static_utils:safe_relative_path("a/../..")),
    ?assertEqual(unsafe, elli_static_utils:safe_relative_path("a/../../..")),
    ?assertEqual(unsafe, elli_static_utils:safe_relative_path("a/./../..")),
    ?assertEqual(unsafe, elli_static_utils:safe_relative_path("a/././../../..")),
    ?assertEqual(unsafe, elli_static_utils:safe_relative_path("a/b/././../../..")),
    ?assertEqual(unsafe, elli_static_utils:safe_relative_path("/root")),

    %% binaries
    ?assertEqual(<<"a">>, elli_static_utils:safe_relative_path(<<"a">>)),
    ?assertEqual(<<"a/b">>, elli_static_utils:safe_relative_path(<<"a/b">>)),
    ?assertEqual(<<"a/b">>, elli_static_utils:safe_relative_path(<<"a/./b">>)),
    ?assertEqual(<<"a/b">>, elli_static_utils:safe_relative_path(<<"a/./b/.">>)),
    ?assertEqual([], elli_static_utils:safe_relative_path(<<"a/..">>)),
    ?assertEqual([], elli_static_utils:safe_relative_path(<<"a/./..">>)),
    ?assertEqual([], elli_static_utils:safe_relative_path(<<"a/../.">>)),
    ?assertEqual(<<"a">>, elli_static_utils:safe_relative_path(<<"a/b/..">>)),
    ?assertEqual(<<"a">>, elli_static_utils:safe_relative_path(<<"a/../a">>)),
    ?assertEqual(<<"a">>, elli_static_utils:safe_relative_path(<<"a/../a/../a">>)),
    ?assertEqual(<<"a/b/c">>, elli_static_utils:safe_relative_path(<<"a/../a/b/c">>)),
    ?assertEqual(unsafe, elli_static_utils:safe_relative_path(<<"a/../..">>)),
    ?assertEqual(unsafe, elli_static_utils:safe_relative_path(<<"a/../../..">>)),
    ?assertEqual(unsafe, elli_static_utils:safe_relative_path(<<"a/./../..">>)),
    ?assertEqual(unsafe, elli_static_utils:safe_relative_path(<<"a/././../../..">>)),
    ?assertEqual(unsafe, elli_static_utils:safe_relative_path(<<"a/b/././../../..">>)),
    ?assertEqual(unsafe, elli_static_utils:safe_relative_path(<<"/root">>)).
