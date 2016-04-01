-module(m_user).
-behaviour(repo_model).

-export([
         schema/0,
         before_save/2,
         after_save/2
        ]).

schema() ->
    #{ fields => #{
           id => #{type => integer, required => true, index => true, readOnly => true},
           login => #{type => {varchar, 255}, required => true}
       },
       table => <<"users">>
    }.

before_save(_C, Model) ->
    catch (test_srv ! {'before', Model}),
    Model.

after_save(_C, Model) ->
    catch (test_srv ! {'after', Model}),
    ok.
