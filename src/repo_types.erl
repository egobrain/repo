-module(repo_types).

-callback get_encoder(Type :: any()) -> fun((A :: any()) -> B :: any()).
-callback get_decoder(Type :: any()) -> fun((B :: any()) -> A :: any()).

-export([
         get_encoder/1,
         get_decoder/1
        ]).

get_encoder(Json) when Json =:= jsonb; Json =:= json -> fun jiffy:encode/1;
get_encoder(_) -> fun id/1.

get_decoder(Json) when Json =:= jsonb; Json =:= json ->
    fun (V) -> jiffy:decode(V, [return_maps]) end;
get_decoder(_) -> fun id/1.

id(A) -> A.
