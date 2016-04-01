-module(repo_model).

-callback schema() -> repo:schema().

-callback before_save(epgpool:connection(), Model) -> Model.
-optional_callbacks([before_save/2]).

-callback after_save(epgpool:connection(), Model :: any()) -> ok.
-optional_callbacks([after_save/2]).
