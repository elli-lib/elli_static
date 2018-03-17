-module(elli_static).

-behaviour(elli_handler).
-behaviour(elli_cache).


-include_lib("elli/include/elli.hrl").
-include_lib("kernel/include/file.hrl").
-include_lib("elli_cache/src/elli_cache_util.hrl").


-define(TABLE, elli_static_table).


-export([preprocess/2,
         handle/2, handle_event/3,
         postprocess/3,
         get_modified/2,
         get_size/2
        ]).

-export_type([config/0]).

-type config() :: [{binary(), {dir, file:name_all()}}].


preprocess(Req, Config)
  when not(?GET_OR_HEAD(Req#req.method)) ->
    MaybeMtime = get_modified(Req, Config),
    MaybeSize = get_size(Req, Config),
    MaybeETag  = maybe_etag(MaybeMtime, MaybeSize),
    rfc7232:init(Req, MaybeMtime, MaybeETag);
preprocess(Req, _Config) ->
    Req.


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


postprocess(Req, {ResponseCode, _Headers, _What} = Res, Config)
  when ?OK_GET_OR_HEAD(ResponseCode, Req#req.method) ->
    MaybeMtime = get_modified(Req, Config),
    MaybeSize = get_size(Req, Config),
    MaybeETag  = maybe_etag(MaybeMtime, MaybeSize),
    rfc7232:init(Req, MaybeMtime, MaybeETag, Res);
postprocess(_, Res, _) ->
    Res.


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
    Size = byte_size(Prefix),
    case elli_request:raw_path(Req) of
        <<Prefix:Size/binary,"/",Path/binary>> ->
            Filename = filename:join(Dir, Path),
            case filelib:is_regular(Filename) of
                true  -> {just, Filename};
                false -> throw({404, [], <<"Not Found">>})
            end;
        _ ->
            nothing
    end.


maybe_etag(nothing, _Size) ->
    nothing;
maybe_etag(_Mtime, nothing) ->
    nothing;
maybe_etag({just, Mtime}, {just, Size}) ->
    ETag = list_to_binary(httpd_util:create_etag(Mtime, Size)),
    {just, <<"\"", ETag/binary, "\"">>}.
