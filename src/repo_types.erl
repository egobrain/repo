-module(repo_types).

-callback get_encoder(Type :: any()) -> fun((A :: any()) -> B :: any()).
-callback get_decoder(Type :: any()) -> fun((B :: any()) -> A :: any()).

-export([
         get_encoder/1,
         get_decoder/1
        ]).

get_encoder(_) -> fun id/1.
get_decoder(_) -> fun id/1.

id(A) -> A.
