-module(elli_static).

-behaviour(elli_handler).

-behaviour(elli_cache).


-include_lib("elli/include/elli.hrl").
-include_lib("kernel/include/file.hrl").

-define(TABLE, elli_static_table).
-define(NOT_FOUND, {404, [], <<"Not Found">>}).

%% elli_handler callbacks
-export([handle/2, handle_event/3]).

%% elli_cache callbacks
-export([get_modified/2, get_size/2]).

-export_type([config/0]).

-type config() :: [{binary(), {dir, file:name_all()}}].


-spec handle(elli:req(), config()) -> elli_handler:result().
handle(_Req, []) ->
    ignore;
handle(Req, [{Prefix, {dir, Dir}}|Args]) ->
    case maybe_file(Req, Prefix, Dir) of
        {just, Filename} ->
            {ok, [], {file, Filename}};
        nothing ->
            handle(Req, Args)
    end.


-spec handle_event(elli_handler:event(), list(), config()) -> ok.
handle_event(elli_startup, _Args, _Config) ->
    ets:new(?TABLE, [set, named_table, public]),
    ok;
handle_event(request_complete, [Req|_Args], _Config) ->
    ReqKey = erlang:phash2(Req),
    ets:delete(?TABLE, ReqKey),
    ok;
handle_event(_Event, _Args, _Config) ->
    ok.


get_modified(Req, Args) ->
    do_it(Req, fun(#file_info{mtime = Mtime}) -> Mtime end, Args).


get_size(Req, Args) ->
    do_it(Req, fun(#file_info{size = Size}) -> Size end, Args).


do_it(_Req, _Fun, []) ->
    nothing;
do_it(Req, Fun, [{Prefix, {dir, Dir}}|Args]) ->
    ReqKey = erlang:phash2(Req),
    case ets:lookup(?TABLE, ReqKey) of
        [{ReqKey, FileInfo}] ->
            {just, Fun(FileInfo)};
        _ ->
            case maybe_file(Req, Prefix, Dir) of
                {just, Filename} ->
                    FileInfo = file_info(Filename),
                    ets:insert(?TABLE, {ReqKey, FileInfo}),
                    {just, Fun(FileInfo)};
                nothing ->
                    do_it(Req, Fun, Args)
            end
    end;
do_it(Req, Fun, [_Arg|Args]) ->
    do_it(Req, Fun, Args).


file_info(Filename) ->
    case file:read_file_info(Filename, [{time, universal}]) of
        {ok, FileInfo} -> FileInfo;
        {error, Reason} -> throw(Reason)
    end.

maybe_file(Req, Prefix, Dir) ->
    %% all paths start with a slash which `safe_relative_path/1' interprets as
    %% unsafe (absolute path), so temporarily remove it
    <<"/", RawPath/binary>> = elli_request:raw_path(Req),

    %% santize the path ensuring the request doesn't access any parent
    %% directories ... and reattach the slash if deemed safe
    SafePath = case filename:safe_relative_path(RawPath) of
                   unsafe ->
                       throw(?NOT_FOUND);
                   %% return type quirk work around
                   [] ->
                       <<"/">>;
                   Sanitized ->
                       <<"/", Sanitized/binary>>
               end,

    Size = byte_size(Prefix),
    case SafePath of
        %% ensure that `SafePath' starts with the correct prefix
        <<Prefix:Size/binary,"/",Path/binary>> ->
            Filename = filename:join(Dir, Path),
            case filelib:is_regular(Filename) of
                true  -> {just, Filename};
                false -> throw(?NOT_FOUND)
            end;
        _ ->
            nothing
    end.
