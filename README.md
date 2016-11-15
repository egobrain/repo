[![Build Status](https://travis-ci.org/egobrain/repo.svg?branch=master)](https://travis-ci.org/egobrain/repo)
[![Coverage Status](https://coveralls.io/repos/github/egobrain/repo/badge.svg?branch=master)](https://coveralls.io/github/egobrain/repo?branch=master)
[![GitHub tag](https://img.shields.io/github/tag/egobrain/repo.svg)](https://github.com/egobrain/repo)
[![Hex.pm](https://img.shields.io/hexpm/v/repo.svg)](https://hex.pm/packages/repo)       

Repo
=====

Repo is a data mapper and DSL on top of [equery](https://github.com/egobrain/equery) 
and [epgpool](https://github.com/egobrain/epgpool) for writing queries and interacting with PostgreSQL databases in Erlang 
inspired by [Ecto](https://github.com/elixir-lang/ecto).

Here is an example:

app.config

```erlang
[
 {epgpool, [
  {database_host, "localhost"},
  {database_name, "mydb"},
  {database_user, "test_user"},
  {database_password, "passwd"}
 ]}
]
```

m_weather.erl

```erlang
-module(m_weather).

-export([schema/0]).

schema() ->
    #{ fields => #{
           city => #{type => {varchar, 255}},
           temp_lo => #{type => integer},
           temp_hi => #{type => integer}
           prcp    => #{type => numeric}
       },
       table => <<"weather">>
    }.
```

repl

```erlang
1> repo:all(m_weather, [
      q:where(fun([#{city := City}]) -> City =:= <<"Kraków">> end),
      %% or repo_utils:like(#{city => <<"Kraków">>}),
      q:order_by(fun([#{temp_lo := T}]) -> [{T, asc}] end),
      q:limit(10)
   ]).
```

Custom db type converters
===

You can implement your own custom dbtype encoders and decoders.
All you need is to implement repo_types behaviour and set it via `PG_TYPES` macro.
Example for rebar3:

```erlang
{overrides, [
    {override, repo, [
        {erl_opts, [
            {d, {'PG_TYPES', my_custom_dbtypes}}
        ]}
    ]}
]}.
```

Notice
===

```A =:= B``` or ```A < B```, or etc syntax is available through parse_transform.  
It's also avaible in repl. But to use in inside modules you need to include  
```-include_lib("equer/include/equery.hrl").``` or use ```pg_sql:'=:='(A, B)``` syntax instead.
Parse transform works only inside ```q``` callbacks.

More documentation will be later...

TODO
===

- [x] ~~common tests~~
- [x] ~~100% coverge~~
- [ ] spec
- [ ] docs
- [ ] examples
