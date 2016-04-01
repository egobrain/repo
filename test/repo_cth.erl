-module(repo_cth).

-export([
         init/2,
         terminate/1,
         pre_init_per_suite/3
        ]).

-include_lib("common_test/include/ct.hrl").

init(_Id, State) ->
    Start = os:timestamp(),
    State2 = pipe([
        fun start_sasl/1,
        fun start_lager/1,
        fun start_epgpool/1,
        fun migrate/1,
        fun start_repo/1
    ], State),
    lager:info("repo started in ~p ms\n",
        [timer:now_diff(os:timestamp(), Start) / 1000]),
    State2.

start_sasl(State) ->
    application:load(sasl),
    application:set_env(kernel, error_logger, silent),
    application:set_env(sasl, sasl_error_logger, {file, "log/sasl.log"}),
    error_logger:tty(false),
    State.

start_lager(State) ->
    application:load(lager),
    application:set_env(lager, log_root, "log"),
    application:set_env(lager, handlers, [
        {lager_console_backend, info}
    ]),
    application:set_env(lager, error_logger_hwm, 256),
    State.

start_epgpool(State) ->
    application:load(epgpool),
    Config = epgpool_cth:start_postgres(),
    epgpool_cth:set_env(Config),
    {ok, _} = application:ensure_all_started(epgpool),
    [{pg_config, Config}|State].

migrate(State) ->
    ok = dbschema:up("../../../../test/migrations"),
    State.

start_repo(State) ->
    application:load(repo),
    application:set_env(repo, fetch_by, 2),
    {ok, _} = application:ensure_all_started(repo),
    State.

pre_init_per_suite(_SuiteName, Config, State) ->
    {Config ++ State, State}.

terminate(State) ->
    ok = epgpool_cth:stop_postgres(?config(pg_config, State)).

%% =============================================================================
%% Internal functions
%% =============================================================================

pipe(Funs, State) ->
    lists:foldl(fun(F, S) -> F(S) end, State, Funs).
