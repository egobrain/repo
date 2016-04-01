-module(repo_utils).

-include_lib("equery/include/equery.hrl").

-export([
         like/1
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
