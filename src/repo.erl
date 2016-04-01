-module(repo).

-export([
         %% Query api
         query/1, query/2,

         all/1, all/2, all/3,
         zlist/2, zlist/3, zlist/4,
         get_one/1, get_one/2, get_one/3,
         insert/2, insert/3,
         update/2, update/3,
         set/2, set/3,
         delete/1, delete/2, delete/3
        ]).

-include_lib("equery/include/equery.hrl").
-include_lib("epgsql/include/epgsql.hrl").

-type schema() :: #{
        fields => #{ atom() => #{} },
        table => binary()
    }.

-export_type([schema/0]).

query(Model) -> query(Model, []).
query(Model, QList) -> pipe(Model, QList).

%% === all/1,2,3 ===============================================================

all(Q) ->
    all_(fun epgpool:with/1, Q).
all(C, Q) when is_pid(C) ->
    all_(wrap_connection(C), Q);
all(Model, QList) ->
    all_(fun epgpool:with/1, Model, QList).
all(C, Model, QList) when is_pid(C) ->
    all_(wrap_connection(C), Model, QList).

all_(FunC, Model, QList) ->
    all_(FunC, query(Model, QList)).

all_(FunC, Q) ->
    {Sql, Args, Fields} = to_sql(qsql:select(Q)),
    {ok, _Columns, Rows} = FunC(fun(C) -> epgsql:equery(C, Sql, Args) end),
    Constructor = get_constructor(Fields),
    [Constructor(R) || R <- Rows].

%% === zlist/2,3,4 =============================================================

zlist(Q, FunZ) ->
    zlist_(fun epgpool:transaction/1, Q, FunZ).
zlist(C, Q, FunZ) when is_pid(C) ->
    zlist_(wrap_connection(C), Q, FunZ);
zlist(Model, QList, FunZ) ->
    zlist_(fun epgpool:transaction/1, query(Model, QList), FunZ).
zlist(C, Model, QList, FunZ) when is_pid(C) ->
    zlist_(wrap_connection(C), query(Model, QList), FunZ).

zlist_(FunC, Q, FunZ) ->
    {Sql, Args, Fields} = to_sql(qsql:select(Q)),
    Constructor = get_constructor(Fields),
    Portal = io_lib:print(make_ref()),
    {ok, NRows} = application:get_env(repo, fetch_by),
    FunC(fun(C) ->
        {ok, Statement} = epgsql:parse(C, Sql),
        ok = epgsql:bind(C, Statement, Portal, Args),
        epgsql:equery(C, Sql, Args),
        try FunZ(zlist(C, Statement, Portal, NRows, Constructor))
        after ok = epgsql:sync(C)
        end
    end).

zlist(C, Statement, Portal, NRows, Constructor) ->
    case epgsql:execute(C, Statement, Portal, NRows) of
        {error, R} ->
            throw({pgsql_exec_error, R});
        {partial, Rows} ->
            ZList = zlist:map(Constructor, zlist:from_list(Rows)),
            zlist:append(ZList, zlist(C, Statement, Portal, NRows, Constructor));
        {ok, []} -> zlist:empty();
        {ok, Rows} -> zlist:map(Constructor, zlist:from_list(Rows))
    end.

%% === get_one/1,2,3 ===========================================================

get_one(Q) ->
    get_one_(fun epgpool:with/1, Q).
get_one(C, Q) when is_pid(C) ->
    get_one_(wrap_connection(C), Q);
get_one(Model, QList) ->
    get_one_(fun epgpool:with/1, Model, QList).
get_one(C, Model, QList) when is_pid(C) ->
    get_one_(wrap_connection(C), Model, QList).

get_one_(FunC, Model, QList) ->
    get_one_(FunC, pipe(Model, QList)).
get_one_(FunC, Q) ->
    case all_(FunC, Q) of
        [] -> {error, not_found};
        [M] -> {ok, M};
        Multiple -> throw({multiple_result, Multiple})
    end.

%% === insert/2,3 ===========================================================

insert(Model, M) ->
    insert_(fun epgpool:transaction/1, Model, M).
insert(C, Model, M) ->
    insert_(wrap_connection(C), Model, M).

insert_(FunC, Model, M) ->
    store(FunC, fun qsql:insert/1, Model, M).

%% === update/2,3 ===========================================================

update(Model, M) ->
    update_(fun epgpool:transaction/1, Model, M).
update(C, Model, M) when is_pid(C) ->
    update_(wrap_connection(C), Model, M).

update_(FunC, Model, M) ->
    store(FunC, fun(Query) ->
       #{fields := Fields} = q:schema(Query),
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
    end, Model, M).

%% === set/2,3 ===========================================================

set(Model, QList) ->
    set_(fun epgpool:transaction/1, Model, QList).
set(C, Model, QList) when is_pid(C) ->
    set_(wrap_connection(C), Model, QList).

set_(FunC, Model, QList) ->
    Q = query(Model, QList),
    #{fields := Fields} = q:schema(Q),
    QR = q:set(fun(S, _) ->
        maps:map(fun(K, V) ->
            Type = maps:get(type, maps:get(K, Fields), undefined),
            (encoder(Type))(V)
        end, S)
    end, Q),
    {Sql, Args, RFields} = to_sql(qsql:update(QR)),
    Constructor = get_constructor(RFields),
    FunC(fun(C) ->
        case epgsql:equery(C, Sql, Args) of
            {ok, 0} -> [];
            {ok, _, _Columns, Rows} -> lists:map(Constructor, Rows)
        end
    end).

%% === delete/1,2,3 ===========================================================

delete(Q) ->
    delete_(fun epgpool:transaction/1, undefined, Q).
delete(C, Q) when is_pid(C) ->
    delete_(wrap_connection(C), undefined, Q);
delete(Model, QList) ->
    delete_(fun epgpool:transaction/1, Model, QList).
delete(C, Model, QList) when is_pid(C) ->
    delete_(wrap_connection(C), Model, QList).

delete_(FunC, Model, QList) when is_list(QList); is_map(QList) ->
    delete_(FunC, Model, pipe(Model, QList));
delete_(FunC, _Model, Q) ->
    {Sql, Args, Fields} = to_sql(qsql:delete(Q)),
    Constructor = get_constructor(Fields),
    FunC(fun(C) when is_pid(C) ->
        Result = epgsql:equery(C, Sql, Args),
        case Result of
            {ok, _} -> [];
            {ok, _, _, Rows} -> lists:map(Constructor, Rows)
        end
    end).

%% =============================================================================
%% Utils
%% =============================================================================

pipe(Model, QList) when is_atom(Model); is_map(Model) ->
    pipe(q:from(Model), QList);
pipe(Query, QList) ->
    q:pipe(Query, where(QList)).

where(QList) when is_list(QList) -> QList;
where(Map) when is_map(Map) ->
    [repo_utils:like(Map)].

maybe_list(M, Fun) when is_list(M) ->
    Fun(M);
maybe_list(M, Fun) ->
    case Fun([M]) of
        {ok, [R]} -> {ok, R};
        {error, [{_N, R}]} -> {error, R}
    end.

store(FunC, SqlF, Model, DataMaybeList) ->
    maybe_list(DataMaybeList, fun(DataList) ->
        store_(FunC, SqlF, Model, DataList)
    end).

store_(FunC, SqlF, Model, DataList) ->
    Query = pipe(Model, [
        fun(Q) ->
            #{fields := Fields} = q:schema(Q),
            q:set(fun(_) ->
                maps:fold(
                    fun (_, #{readOnly := true}, S) -> S;
                        (F, Opts, S) -> maps:put(F, {F, Opts}, S)
                    end, #{}, Fields)
            end, Q)
        end
    ]),
    {Sql, ArgsFields, ReturnFieldsData} = to_sql(SqlF(Query)),
    BeforeHook = get_hook(Model, before_save),
    AfterHook = get_hook(Model, after_save),
    Constructor = get_constructor(ReturnFieldsData),
    FunC(fun(C) ->
        {ok, S} = epgsql:parse(C, Sql),
        QueryData = [{S, data(ArgsFields, BeforeHook(C, M))} || M <- DataList],
        QueryResult = epgsql:execute_batch(C, QueryData),
        enumerate_error_writer_map(
            fun ({ok, 0}) -> {error, not_found};
                ({ok, _, [R]}) ->
                    Result = Constructor(R),
                    ok = AfterHook(C, Result),
                    {ok, Result};
                ({error, #error{code = <<"23505">>,codename=unique_violation}}) ->
                    {error, duplicate};
                ({error, duplicate}=Err) -> Err
            end, QueryResult)
    end).

get_constructor(FieldsData) when is_list(FieldsData) ->
    FieldsConverters = [
        {F, decoder(maps:get(type, Opts, undefined))} || {F, Opts} <- FieldsData
    ],
    fun(TupleData) ->
        maps:from_list(
            lists:zipwith(
                fun({F,C}, D) -> {F, C(D)} end,
                FieldsConverters,
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

get_hook(Model, before_save) ->
    case erlang:function_exported(Model, before_save, 2) of
        true -> fun Model:before_save/2;
        false -> fun(_, M) -> M end
    end;
get_hook(Model, after_save) ->
    case erlang:function_exported(Model, after_save, 2) of
        true -> fun Model:after_save/2;
        false -> fun(_, _) -> ok end
    end.

encoder(json) -> fun jiffy:encode/1;
encoder(_) -> fun id/1.

decoder(json) -> fun (null) -> null; (V) -> jiffy:decode(V, [return_maps]) end;
decoder({record, FieldsData}) -> get_constructor(FieldsData);
decoder({array, SubType}) ->
    TypeDecoder = decoder(SubType),
    fun(D) -> lists:map(TypeDecoder, D) end;
decoder(_) -> fun id/1.

id(A) -> A.

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
