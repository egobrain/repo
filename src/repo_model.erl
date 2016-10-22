-module(repo_model).

-callback schema() -> repo:schema().

-callback before_save(epgpool:connection(), Model) -> Model.
-optional_callbacks([before_save/2]).

-callback after_save(epgpool:connection(), Model :: any()) -> ok.
-optional_callbacks([after_save/2]).

-callback from_db([FieldNames :: atom()]) -> fun((Data :: [any()]) -> Model :: any()).
-optional_callbacks([from_db/1]).

-callback to_db(Model :: any()) -> DbModel :: #{ atom() => any() }.
-optional_callbacks([to_db/1]).

-export([
         before_save/2,
         after_save/2,
         from_db/1,
         to_db/1
        ]).

before_save(_C, Model) -> Model.
after_save(_C, _Model) -> ok.

from_db(Fields) ->
    fun(Values) -> maps:from_list(lists:zip(Fields, Values)) end.

to_db(Model) -> Model.
