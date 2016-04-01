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

Notice
===

```A =:= B``` or ```A < B eg```, or etc syntax is available through parse_transform.  
It's also avaible in repl. But to use in inside modules you need to include  
```-include_lib("equer/include/equery.hrl").``` or use ```q:'=:='(A, B)``` syntax instead.
Parse transform works only inside ```q``` callbacks.

More documentation will be later...

TODO
===

- [x] common tests
- [ ] 100% coverge
- [ ] spec
- [ ] docs
- [ ] examples
