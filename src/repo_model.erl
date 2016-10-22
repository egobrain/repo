-module(repo_model).

-callback schema() -> repo:schema().

-callback before_save(epgpool:connection(), Model, HookOpts :: any()) -> {ok, Model} | {error, Reason :: any()}.
-optional_callbacks([before_save/3]).

-callback after_save(epgpool:connection(), OldModel :: any(), NewModel, HookOpts :: any()) -> NewModel.
-optional_callbacks([after_save/4]).

-callback before_delete(epgpool:connection(), q:query(), HookOpts :: any()) -> {ok, q:query()} | {error, Reason :: any()}.
-optional_callbacks([before_delete/3]).

-callback after_delete(epgpool:connection(), DeletedModel :: any(), HookOpts :: any()) -> Model :: any().
-optional_callbacks([after_delete/3]).

-callback from_db([FieldNames :: atom()]) -> fun((Data :: [any()]) -> Model :: any()).
-optional_callbacks([from_db/1]).

-callback to_db(Model :: any()) -> DbModel :: #{ atom() => any() }.
-optional_callbacks([to_db/1]).

-export([
         before_save/3,
         after_save/4,
         before_delete/3,
         after_delete/3,
         from_db/1,
         to_db/1
        ]).

before_save(_C, Model, _HookOpts) -> {ok, Model}.
after_save(_C, _BeforeModel, AfterModel, _HookOpts) -> AfterModel.

before_delete(_C, Q, _HookOpts) -> {ok, Q}.
after_delete(_C, DeletedModel, _HookOpts) -> DeletedModel.

from_db(Fields) -> fun(Values) -> maps:from_list(lists:zip(Fields, Values)) end.

to_db(Model) -> Model.
