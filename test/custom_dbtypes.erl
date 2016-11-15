-module(custom_dbtypes).

-behavior(repo_types).

-export([
         get_encoder/1,
         get_decoder/1
        ]).

get_encoder(Json) when Json =:= jsonb; Json =:= json -> fun jiffy:encode/1;
get_encoder(point) ->
    fun({Lat, Long}) ->
        iolist_to_binary(io_lib:format("(~p, ~p)", [Long, Lat]))
    end;
get_encoder(_) -> fun id/1.

get_decoder(Json) when Json =:= jsonb; Json =:= json ->
    fun (V) -> jiffy:decode(V, [return_maps]) end;
get_decoder(point) -> fun({Long, Lat}) -> {Lat, Long} end;
get_decoder(_) -> fun id/1.

id(A) -> A.
