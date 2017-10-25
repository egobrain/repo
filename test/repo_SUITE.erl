-module(repo_SUITE).

-export([
         init_per_suite/1,
         end_per_suite/1,
         all/0,

         all_test/1,
         zlist_test/1,
         single_item_test/1,
         get_one_test/1,
         insert_test/1,
         manual_on_conflict_test/1,
         upsert_test/1,
         update_test/1,
         delete_test/1,
         query_test/1,
         preload_test/1,
         set_test/1,
         hooks_test/1,
         errors_test/1,
         db_types_test/1
        ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("equery/include/equery.hrl").

-record(user, {id, login, tag = null}).

init_per_suite(Config)  ->
    Config.

end_per_suite(_Config) ->
    ok.

all() ->
    [
     all_test,
     zlist_test,
     single_item_test,
     get_one_test,
     insert_test,
     manual_on_conflict_test,
     upsert_test,
     update_test,
     delete_test,
     query_test,
     preload_test,
     set_test,
     hooks_test,
     errors_test,
     db_types_test
    ].

all_test(_Config) ->
    [
     #user{id = 1, login = <<"Sam">>},
     #user{ id = 2, login = <<"Mike">>},
     #user{ id = 3, login = <<"Joe">>},
     #user{ id = 4, login = <<"Elis">>}
    ] = repo:all(m_user, []),

    [
     #user{id = 1, login = <<"Sam">>},
     #user{id = 2, login = <<"Mike">>}
    ] = repo:all(m_user, [
        q:where(fun([#{id := Id}]) -> Id < 3 end)
    ]),

    QList = [
        q:order_by(fun([#{id := Id}]) -> [{Id, asc}] end),
        q:limit(1)
    ],
    Q = repo:query(m_user, QList),

    [
     #user{ id = 1 }
    ] = epgpool:with(fun(C) -> repo:all(C, Q) end),

    [
     #user{ id = 1 }
    ] = epgpool:with(fun(C) -> repo:all(C, m_user, QList) end).

zlist_test(_Config) ->
    [
     #user{id = 1, login = <<"Sam">>},
     #user{id = 2, login = <<"Mike">>},
     #user{id = 3, login = <<"Joe">>},
     #user{id = 4, login = <<"Elis">>}
    ] = repo:zlist(m_user, [], fun zlist:to_list/1),

    QList = [
        q:order_by(fun([#{id := Id}]) -> [{Id, asc}] end),
        q:limit(1)
    ],
    Q = repo:query(m_user, QList),

    [
     #user{id = 1 }
    ] = repo:zlist(Q, fun zlist:to_list/1),

    [
     #user{id = 1 }
    ] = epgpool:transaction(fun(C) -> repo:zlist(C, Q, fun zlist:to_list/1) end),

    [
     #user{id = 1 }
    ] = epgpool:transaction(fun(C) -> repo:zlist(C, m_user, QList, fun zlist:to_list/1) end),

    try
        must_throw_exception = repo:zlist(m_user, [
            q:select(fun([#{id := UserId}]) -> #{id => UserId / 0} end)
        ], fun zlist:to_list/1)
    catch throw:{pgsql_exec_error, _} -> ok
    end.

single_item_test(_Config) ->
    {ok, 4} = repo:get_one(m_user, [q:select(fun([#{id := Id}]) -> pg_sql:max(Id) end)]).

get_one_test(_Config) ->
    {ok, #user{login = <<"Sam">>}} =
        repo:get_one(m_user, #{id => 1, unknwon => <<"must be ignored">>}),
    {error, not_found} = repo:get_one(m_user, #{id => -1}),
    Q = repo:query(m_user, #{id => 1}),
    {ok, _} = repo:get_one(Q),
    {ok, _} = epgpool:with(fun(C) -> repo:get_one(C, Q) end),
    {ok, _} = epgpool:with(fun(C) -> repo:get_one(C, m_user, #{id => 1}) end).

insert_test(_Config) ->
    {ok, [#user{id = Id, login = <<"Yakov">>}=U]} =
         repo:insert(m_user, [#user{login = <<"Yakov">>, id = <<"ignored">>}]),
    {ok, U} = repo:get_one(m_user, #{id => Id}),
    {ok, [#user{}, #user{}]} = epgpool:with(fun(C) ->
         repo:insert(C, m_user, [
             #user{ login = <<"insert1">>},
             #user{ login = <<"insert2">>}
         ])
    end),
    {ok, []} = repo:insert(m_user, []),
    {error, r1} = repo:insert(m_user, #user{login = <<"Yk">>}, #{error => r1}),
    {error, [{1, r1}]} = repo:insert(m_user, [#user{login = <<"Yk">>}], #{error => r1}).

manual_on_conflict_test(_Config) ->
    {ok, [#user{id = Id, login = <<"Ins">>}=U]} =
        repo:insert(m_user, [#user{login = <<"Ins">>, id = <<"ignored">>}]),
    NewLogin = <<"NewIns">>,
    {ok, [#user{id = Id, login = NewLogin}=U2]} =
        repo:insert(repo:query(m_user, [
            q:on_conflict([login], fun([#{id := OldId}, Excluded]) ->
                Excluded#{id => OldId, login => NewLogin}
            end)
        ]), [U]),
    {ok, U2} = repo:get_one(m_user, #{id => Id}).

upsert_test(_Config) ->
    {ok, [#{id := Id, value := <<"up_created">>}=U]} =
         repo:upsert(m_setting, [#{id => 1, value => <<"up_created">>}]),
    {ok, U} = repo:get_one(m_setting, #{id => Id}),
    UpdatedU = U#{ value => <<"up_upsert">> },
    NewU = #{ id => 2, value => <<"up_insert">> },
    {ok, [UpdatedU, #{value := <<"up_insert">>}]} = epgpool:with(fun(C) ->
         repo:upsert(C, m_setting, [
             UpdatedU,
             NewU
         ])
    end).

update_test(_Config) ->
    Text = <<"Sql is a great thing!">>,
    {ok, Comment} = repo:get_one(m_comment, #{id => 1}),
    {ok, #{text := Text}} = repo:update(m_comment, Comment#{text := Text}),
    {ok, #{text := Text}} = repo:get_one(m_comment, #{id => 1}),

    Text2 = <<"Sql is a great thing!!!">>,
    {ok, #{text := Text2}} =
        epgpool:transaction(fun(C) ->
            repo:update(C, m_comment, Comment#{text := Text2})
        end),
    {ok, #{text := Text2}} = repo:get_one(m_comment, #{id => 1}).

delete_test(_Config) ->
    {error, r2} = repo:delete(m_user, #{id => 3}, #{error => r2}),
    {ok, [#user{id = 3}]} = repo:delete(m_user, #{id => 3}),
    {error, not_found} = repo:get_one(m_user, #{id => 3}),
    {ok, #user{id=U1Id}=U1} = repo:insert(m_user, #user{login = <<"to delete 1">>}),
    {ok, [U1]} = epgpool:with(fun(C) -> repo:delete(C, m_user, #{id => U1Id}) end),
    {ok, U2} = repo:insert(m_user, #user{login = <<"to delete 2">>}),
    Q = repo:query(m_user, [
        q:where(fun([#{login := Login}]) -> Login =:= <<"to delete 2">> end)
    ]),
    {ok, Post} = repo:insert(m_post, #{
        header => <<"to delete">>,
        text => <<"Linked post">>,
        author_id => U2#user.id
    }),
    {error, _} = repo:delete(Q),
    {ok, [_]} = repo:delete(m_post, #{id => maps:get(id, Post)}),
    {ok, [U2]} = repo:delete(Q),
    {ok, []} = repo:delete(Q),

    {ok, U3} = repo:insert(m_user, #user{login = <<"to delete 3">>}),
    Q2 = repo:query(m_user, [
        q:where(fun([#{login := Login}]) -> Login =:= <<"to delete 3">> end)
    ]),
    {ok, [U3]} = epgpool:with(fun(C) -> repo:delete(C, Q2) end).

query_test(_Config) ->
    Q = repo:query(m_user),
    Q2 = q:where(fun([#{id := Id}]) -> Id > 1 end, Q),
    Q3 = q:where(fun([#{id := Id}]) -> Id < 4 end, Q2),
    Q4 = repo:query(Q3, [
        q:limit(1),
        q:order_by(fun([#{id := Id}]) -> [{Id, asc}] end)
    ]),
    [#user{id = 2}] = repo:all(Q4).

set_test(_Config) ->
    [#{meta := #{<<"type">> := <<"comment">>}}|_] =
        repo:set(m_comment, [
            q:set(fun(_) -> #{meta => #{'type' => comment}} end)
        ]),
    [#{meta := #{<<"type">> := <<"comment">>}}|_] =
        epgpool:transaction(fun(C) ->
            repo:set(C, m_comment, [
                q:set(fun(_) -> #{meta => #{'type' => comment}} end)
            ])
        end),
    [] = repo:set(m_comment, [
        q:set(fun(_) -> #{meta => #{'type' => comment}} end),
        q:where(fun([#{id := Id}]) -> Id > 999 end)
    ]).


preload_test(_Config) ->
    [
     #{
         id := 1,
         header := <<"About sql">>,
         text := <<"foo">>,
         author := #user{
             id = 1,
             login = <<"Sam">>
         },
         comments := [
             #{
                 id := 1,
                 text := <<"Sql is a great thing!!!">>,
                 author := #user{
                     login = <<"Mike">>
                 }
             },
             #{
                 id := 2,
                 text := <<"What is sql?">>,
                 author := #user{
                     login = <<"Elis">>
                 }
             }
         ]
     },
     #{
         id := 3,
         header := <<"My cookies">>,
         text := <<"Ooops">>,
         author := #user{
             id = 4,
             login = <<"Elis">>
         },
         comments := [
             #{
                 id := 3,
                 text := <<"Teasty?">>,
                 author := #user{
                     login = <<"Sam">>
                 }
             },
             #{
                 id := 4,
                 text := <<"Great!">>,
                 author := #user{
                     login = <<"Elis">>
                 }
             }
         ]
     }
    ] = repo:all(m_post, [
        repo_utils:preload(author),
        repo_utils:preload(comments, [
            repo_utils:preload(author, [q:select(fun([T]) -> maps:with([login], T) end)]),
            q:select(fun(S, _) -> maps:with([id, text, author], S) end),
            q:order_by(fun([#{id := Id}]) -> [{Id, asc}] end)
        ]),
        q:where(fun([#{id := Id}]) -> Id =:= 1 orelse Id =:= 3 end),
        q:select(fun(S, _) -> maps:without([author_id], S) end)
    ]).

hooks_test(_Config) ->
    register(test_srv, self()),
    Model = #user{login = <<"Hook">>},
    {ok, M} = repo:insert(m_user, Model),
    {'before', Model} = receive D1 -> D1 after 1000 -> throw(timeout) end,
    {'after', M} = receive D2 -> D2 after 1000 -> throw(timeout) end.


errors_test(_Config) ->
    try
        must_throw_exception = repo:zlist(m_user, [
            q:select(fun(_) -> #{id => qast:raw(<<"bad sql">>)} end)
        ], fun zlist:to_list/1)
    catch throw:{pgsql_exec_error, _} -> ok
    end,

    try
        must_throw_exception = repo:get_one(m_user, [])
    catch throw:{multiple_result, _} -> ok
    end,

    {error, duplicate} = repo:insert(m_user, #user{login = <<"Sam">>}),
    {error, [{1, duplicate}]} = repo:insert(m_user, [#user{login = <<"Sam">>}]),
    {error, _} = repo:insert(m_user, #user{}),

    {error, not_found} = repo:update(m_user, #user{id = 999, login = <<"Samson">>}).

db_types_test(_) ->
    %% Encoders
    1 = (repo_types:get_encoder(any))(1),

    %% Decoders
    1 = (repo_types:get_decoder(any))(1).
