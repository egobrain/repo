-module(m_post).
-behaviour(repo_model).

-export([schema/0]).

schema() ->
    #{ fields => #{
           id => #{type => integer, required => true, index => true, readOnly => true},
           header => #{type => {varchar, 255}, required => true},
           text => #{type => text, required => true},
           author_id => #{type => integer}
       },
       table => <<"posts">>,
       links => #{
           author => {belongs_to, m_user, #{author_id => id}},
           comments => {has_many, m_comment, #{id => post_id}}
       }
    }.
