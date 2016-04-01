-module(m_comment).
-behaviour(repo_model).

-export([schema/0]).

schema() ->
    #{ fields => #{
           id => #{type => integer, required => true, index => true, readOnly => true},
           text => #{type => text, required => true},
           author_id => #{type => integer},
           post_id => #{type => integer},
           meta => #{type => json}
       },
       table => <<"comments">>,
       links => #{
           author => {belongs_to, m_user, #{author_id => id}}
       }
    }.
