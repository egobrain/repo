-module(repo_model).

-callback schema() -> repo:schema().

-callback before_save(epgpool:connection(), Model) -> Model.
-callback after_save(epgpool:connection(), Model :: any()) -> ok.

-optional_callback([before_save/3, after_save/2]).
