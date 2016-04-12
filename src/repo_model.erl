-module(repo_model).

-callback schema() -> repo:schema().

-callback before_save(epgpool:connection(), Model) -> Model.
-optional_callbacks([before_save/2]).

-callback after_save(epgpool:connection(), Model :: any()) -> ok.
-optional_callbacks([after_save/2]).

-export([
         before_save/2,
         after_save/2
        ]).

before_save(_C, Model) -> Model.
after_save(_C, _Model) -> ok.
