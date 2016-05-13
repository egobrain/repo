-module(m_setting).
-behaviour(repo_model).

-export([schema/0]).

schema() ->
    #{ fields => #{
           id => #{type => integer, required => true, index => true},
           value => #{type => {varchar, 255}, required => true}
       },
       table => <<"kv">>
    }.
