-module(repo_utils).

-include_lib("equery/include/equery.hrl").

-export([
         like/1,
         preload/1, preload/2
        ]).

%% =============================================================================
%% Query utils
%% =============================================================================

like(Map) ->
    q:where(fun([M|_]) ->
        maps:fold(
            fun(K, V, S) ->
                case maps:find(K, M) of
                    {ok, V2} -> S andalso V =:= V2;
                    error -> S
                end
            end, true, Map)
    end).

preload(Link) -> preload(Link, []).
preload(Link, QList) ->
    fun(Q) -> preload(Link, QList, Q) end.

preload(Link, QList, Q) ->
    Links = maps:get(links, q:get(schema, Q), #{}),
    q:select(fun(Select, [MD|_]) ->
        {LinkType, Info, IdsMap} = maps:get(Link, Links),
        SubQ = repo:query(Info, [
            q:where(fun([LinkD|_]) ->
                maps:fold(fun(MField, LinkField, S) ->
                    S andalso pg_sql:'=:='(maps:get(MField, MD), maps:get(LinkField, LinkD))
                end, true, IdsMap)
            end)
        ]),
        SubQ2 = q:pipe(SubQ, QList),
        SubSchema = q:get(schema, SubQ2),
        SubModel = maps:get(model, SubSchema, undefined),
        SubQR = q:select(fun(SubSelect, _Data) -> pg_sql:row(SubModel, SubSelect) end, SubQ2),
        Exp = qsql:select(SubQR),
        LinkExp =
            case LinkType of
                has_many -> array(Exp);
                belongs_to -> braced(Exp)
            end,
        maps:put(Link, LinkExp, Select)
    end, Q).

array(Ast) ->
    Opts = qast:opts(Ast),
    Type = maps:get(type, Opts, undefined),
    qast:exp([
        qast:raw("ARRAY("),
        Ast,
        qast:raw(")")
    ], Opts#{type => {array, Type}}).

braced(Ast) ->
    Opts = qast:opts(Ast),
    qast:exp([
        qast:raw("("),
        Ast,
        qast:raw(")")
    ], Opts).
