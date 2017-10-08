-module(repo).

-export([
         %% Query api
         query/1, query/2,

         all/1, all/2, all/3,
         zlist/2, zlist/3, zlist/4,
         get_one/1, get_one/2, get_one/3,
         insert/2, insert/3, insert/4,
         upsert/2, upsert/3, upsert/4,
         update/2, update/3, update/4,
         set/2, set/3,
         delete/1, delete/2, delete/3, delete/4
        ]).

-include_lib("equery/include/equery.hrl").
-include_lib("epgsql/include/epgsql.hrl").

-include("config.hrl").

query(Model) -> query(Model, []).
query(Model, QList) -> pipe(Model, QList).

%% === all/1,2,3 ===============================================================

all(Info) ->
    all_(fun epgpool:with/1, query(Info)).
all(C, Info) when is_pid(C) ->
    all_(wrap_connection(C), query(Info));
all(Info, QList) ->
    all_(fun epgpool:with/1, query(Info, QList)).
all(C, Info, QList) when is_pid(C) ->
    all_(wrap_connection(C), query(Info, QList)).

all_(FunC, Query) ->
    {Sql, Args, Fields} = to_sql(qsql:select(Query)),
    {ok, _Columns, Rows} = FunC(fun(C) -> epgsql:equery(C, Sql, Args) end),
    Constructor = get_constructor(Fields),
    [Constructor(R) || R <- Rows].

%% === zlist/2,3,4 =============================================================

zlist(Info, FunZ) ->
    zlist_(fun epgpool:transaction/1, query(Info), FunZ).
zlist(C, Info, FunZ) when is_pid(C) ->
    zlist_(wrap_connection(C), query(Info), FunZ);
zlist(Info, QList, FunZ) ->
    zlist_(fun epgpool:transaction/1, query(Info, QList), FunZ).
zlist(C, Info, QList, FunZ) when is_pid(C) ->
    zlist_(wrap_connection(C), query(Info, QList), FunZ).

zlist_(FunC, Q, FunZ) ->
    {Sql, Args, Fields} = to_sql(qsql:select(Q)),
    Constructor = get_constructor(Fields),
    Portal = io_lib:print(make_ref()),
    {ok, NRows} = application:get_env(repo, fetch_by),
    FunC(fun(C) ->
        case epgsql:parse(C, Sql) of
            {ok, Statement} ->
                ok = epgsql:bind(C, Statement, Portal, Args),
                try FunZ(zlist(C, Statement, Portal, NRows, Constructor))
                after ok = epgsql:sync(C)
                end;
            {error, R} -> throw({pgsql_exec_error, R})
        end
    end).

zlist(C, Statement, Portal, NRows, Constructor) ->
    case epgsql:execute(C, Statement, Portal, NRows) of
        {error, R} ->
            throw({pgsql_exec_error, R});
        {partial, Rows} ->
            ZList = zlist:map(Constructor, zlist:from_list(Rows)),
            zlist:append(ZList, fun() -> (zlist(C, Statement, Portal, NRows, Constructor))() end);
        {ok, []} -> zlist:empty();
        {ok, Rows} -> zlist:map(Constructor, zlist:from_list(Rows))
    end.

%% === get_one/1,2,3 ===========================================================

get_one(Info) ->
    get_one_(fun epgpool:with/1, query(Info)).
get_one(C, Info) when is_pid(C) ->
    get_one_(wrap_connection(C), query(Info));
get_one(Info, QList) ->
    get_one_(fun epgpool:with/1, query(Info, QList)).
get_one(C, Info, QList) when is_pid(C) ->
    get_one_(wrap_connection(C), query(Info, QList)).

get_one_(FunC, Q) ->
    case all_(FunC, Q) of
        [] -> {error, not_found};
        [M] -> {ok, M};
        Multiple -> throw({multiple_result, Multiple})
    end.

%% === insert/2,3,4 =========================================================

insert(Info, M) ->
    insert_(fun epgpool:transaction/1, Info, M, undefined).
insert(C, Info, M) when is_pid(C) ->
    insert_(wrap_connection(C), Info, M, undefined);
insert(Info, M, HookOpts) ->
    insert_(fun epgpool:transaction/1, Info, M, HookOpts).
insert(C, Info, M, HookOpts) ->
    insert_(wrap_connection(C), Info, M, HookOpts).

insert_(FunC, Info, M, HookOpts) ->
    store(FunC, fun qsql:insert/1, Info, M, HookOpts).

%% === upsert/2,3,4 =========================================================

upsert(Info, M) ->
    upsert(Info, M, undefined).
upsert(C, Info, M) when is_pid(C) ->
    upsert(C, Info, M, undefined);
upsert(Info, M, HookOpts) ->
    upsert_(fun epgpool:transaction/1, Info, M, HookOpts).
upsert(C, Info, M, HookOpts) ->
    upsert_(wrap_connection(C), Info, M, HookOpts).

upsert_(FunC, Info, M, HookOpts) ->
    Query = query(Info),
    #{fields := Fields} = q:get(schema, Query),
    IndexFields = maps:fold(
        fun (K, #{index := true}, Acc) -> [K|Acc];
            (_, _, Acc) -> Acc
        end, [], Fields),
    FieldsToUpdate = maps:fold(
        fun (_, #{readOnly := true}, Acc) -> Acc;
            (K, _, Acc) -> [K|Acc]
        end, [], Fields),

    QueryR = q:on_conflict(IndexFields, fun(Data) ->
        maps:with(FieldsToUpdate, lists:last(Data))
    end, Query),
    insert_(FunC, QueryR, M, HookOpts).

%% === update/2,3 ===========================================================

update(Info, M) ->
    update_(fun epgpool:transaction/1, Info, M, undefined).
update(C, Info, M) when is_pid(C) ->
    update_(wrap_connection(C), Info, M, undefined);
update(Info, M, HookOpts) ->
    update_(fun epgpool:transaction/1, Info, M, HookOpts).
update(C, Info, M, HookOpts) ->
    update_(wrap_connection(C), Info, M, HookOpts).

update_(FunC, Info, M, HookOpts) ->
    store(FunC, fun(Query) ->
       #{fields := Fields} = q:get(schema, Query),
        Q = pipe(Query, [
            q:where(fun([Data|_]) ->
                maps:fold(
                    fun (IndexF, #{index := true}=Opts, S) ->
                            S andalso maps:get(IndexF, Data) =:= {IndexF, Opts};
                        (_, _, S) -> S
                    end, true, Fields)
                end)
        ]),
        qsql:update(Q)
    end, Info, M, HookOpts).

%% === set/2,3 ===========================================================

set(Info, QList) ->
    set_(fun epgpool:transaction/1, query(Info, QList)).
set(C, Info, QList) when is_pid(C) ->
    set_(wrap_connection(C), query(Info, QList)).

set_(FunC, Query) ->
    #{fields := Fields} = q:get(schema, Query),
    QR = q:set(fun(S, _) ->
        maps:map(fun(K, V) ->
            Type = maps:get(type, maps:get(K, Fields), undefined),
            (encoder(Type))(V)
        end, S)
    end, Query),
    {Sql, Args, RFields} = to_sql(qsql:update(QR)),
    Constructor = get_constructor(RFields),
    FunC(fun(C) ->
        case epgsql:equery(C, Sql, Args) of
            {ok, 0} -> [];
            {ok, _, _Columns, Rows} -> lists:map(Constructor, Rows)
        end
    end).

%% === delete/1,2,3,4 =========================================================

delete(Info) ->
    delete_(fun epgpool:transaction/1, query(Info), undefined).
delete(C, Info) when is_pid(C) ->
    delete_(wrap_connection(C), query(Info), undefined);
delete(Info, QList) ->
    delete_(fun epgpool:transaction/1, query(Info, QList), undefined).
delete(C, Info, QList) when is_pid(C) ->
    delete_(wrap_connection(C), query(Info, QList), undefined);
delete(Info, QList, HookOpts) ->
    delete_(fun epgpool:transaction/1, query(Info, QList), HookOpts).
delete(C, Info, QList, HookOpts) ->
    delete_(wrap_connection(C), query(Info, QList), HookOpts).

delete_(FunC, Query, HookOpts) ->
    Model = get_model(Query),
    BeforeHook = get_hook(Model, before_delete, 3),
    AfterHook = get_hook(Model, after_delete, 3),
    FunC(fun(C) when is_pid(C) ->
        case BeforeHook(C, Query, HookOpts) of
            {ok, Q1} ->
                {Sql, Args, Fields} = to_sql(qsql:delete(Q1)),
                Constructor = get_constructor(Fields),
                Result = epgsql:equery(C, Sql, Args),
                case Result of
                    {ok, _} -> {ok, []};
                    {ok, _, _, Rows} ->
                        {ok, [AfterHook(C, M, HookOpts) || M <- lists:map(Constructor, Rows)]};
                    {error, _} = V -> V

                end;
            {error, _} = V -> V
        end
    end).

%% =============================================================================
%% Utils
%% =============================================================================

get_model(Q) ->
    maps:get(model, q:get(schema, Q), undefined).

pipe(Info, QList) when is_atom(Info); is_map(Info) ->
    pipe(q:from(Info), QList);
pipe(Query, QList) ->
    q:pipe(Query, where(QList)).

where(QList) when is_list(QList) -> QList;
where(Map) when is_map(Map) ->
    [repo_utils:like(Map)].

maybe_list([], _Fun) ->
    {ok, []};
maybe_list(M, Fun) when is_list(M) ->
    Fun(M);
maybe_list(M, Fun) ->
    case Fun([M]) of
        {ok, [R]} -> {ok, R};
        {error, [{_N, R}]} -> {error, R}
    end.

store(FunC, SqlF, Info, DataMaybeList, HookOpts) ->
    maybe_list(DataMaybeList, fun(DataList) ->
        store_(FunC, SqlF, Info, DataList, HookOpts)
    end).

store_(FunC, SqlF, Info, DataList, HookOpts) ->
    Query = pipe(Info, [
        fun(Q) ->
            #{fields := Fields} = q:get(schema, Q),
            q:set(fun(_) ->
                maps:fold(
                    fun (_, #{readOnly := true}, S) -> S;
                        (F, Opts, S) -> maps:put(F, {F, Opts}, S)
                    end, #{}, Fields)
            end, Q)
        end
    ]),
    {Sql, ArgsFields, ReturnFieldsData} = to_sql(SqlF(Query)),
    Model = get_model(Query),
    BeforeHook = get_hook(Model, before_save, 3),
    AfterHook = get_hook(Model, after_save, 4),
    ToDb = get_hook(Model, to_db, 1),
    Constructor = get_constructor(ReturnFieldsData),
    FunC(fun(C) ->
        case enumerate_error_writer_map(fun(M) -> BeforeHook(C, M, HookOpts) end, DataList) of
            {ok, PreprocessedData} ->
                PreparedData = [data(ArgsFields, ToDb(M)) || M <- PreprocessedData],
                {ok, S} = epgsql:parse(C, Sql),
                QueryData = [{S, M} || M <- PreparedData],
                QueryResult = epgsql:execute_batch(C, QueryData),
                enumerate_error_writer_map(
                    fun ({{ok, 0}, _}) -> {error, not_found};
                        ({{ok, 0, []}, _}) -> {error, not_found};
                        ({{ok, _, [R]}, M}) ->
                            Result = Constructor(R),
                            UpdatedResult = AfterHook(C, M, Result, HookOpts),
                            {ok, UpdatedResult};
                        ({{error, #error{code = <<"23505">>,codename=unique_violation}}, _}) ->
                            {error, duplicate};
                        ({{error, _Reason}=Err, _}) -> Err
                    end, lists:zip(QueryResult, PreprocessedData));
            V -> V
        end
    end).

get_constructor({model, Model, FieldsData}) when is_list(FieldsData) ->
    {Fields, FieldsOpts} = lists:unzip(FieldsData),
    Decoders = lists:map(fun(Opts) ->
        decoder(maps:get(type, Opts, undefined))
    end, FieldsOpts),
    FromDb = (get_hook(Model, from_db, 1))(Fields),
    fun(TupleData) ->
        FromDb(
            lists:zipwith(
                fun(C, D) -> C(D) end,
                Decoders,
                tuple_to_list(TupleData)))
    end;
get_constructor(FieldType) ->
    Decoder = decoder(FieldType),
    fun({V}) -> Decoder(V) end.

data(FieldsData, M) ->
    lists:map(fun({F, Opts}) ->
        case maps:find(F, M) of
            {ok, V} ->
                Encoder = encoder(maps:get(type, Opts, undefined)),
                Encoder(V);
            _ -> null
        end
    end, FieldsData).

get_hook(Model, Name, Arity) when is_atom(Model) ->
    Module = case erlang:function_exported(Model, Name, Arity) of
        true -> Model;
        false -> repo_model
    end,
    fun Module:Name/Arity.

encoder(Type) ->
    Encoder = ?PG_TYPES:get_encoder(Type),
    fun (null) -> null; (V) -> Encoder(V) end.

decoder(Type) ->
    Decoder = decoder_(Type),
    fun (null) -> null; (V) -> Decoder(V) end.
decoder_({record, FieldsData}) -> get_constructor(FieldsData);
decoder_({array, SubType}) ->
    TypeDecoder = decoder(SubType),
    fun(D) -> lists:map(TypeDecoder, D) end;
decoder_(Type) -> ?PG_TYPES:get_decoder(Type).

to_sql(QAst) ->
    {Sql, Args} = qast:to_sql(QAst),
    #{type := Fields} = qast:opts(QAst),
    {iolist_to_binary(Sql), Args, Fields}.

enumerate_error_writer_map(Fun, List) ->
    {_, Result, Errors} = lists:foldl(fun(E, {N, Ok, Err}) ->
        N2 = N+1,
        case Fun(E) of
            {ok, R} -> {N2, [R|Ok], Err};
            {error, R} -> {N2, Ok, [{N2,R}|Err]}
        end
    end, {0, [], []}, List),
    case Errors of
        [] -> {ok, lists:reverse(Result)};
        _ -> {error, lists:reverse(Errors)}
    end.

-spec wrap_connection(C) -> fun((fun((C) -> R)) -> R) when
        C :: epgpool:connection().
wrap_connection(C) -> fun(F) -> F(C) end.
