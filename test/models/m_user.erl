-module(m_user).
-behaviour(repo_model).

-export([
         schema/0,
         before_save/2,
         after_save/2,

         from_db/1,
         to_db/1
        ]).

-record(user, {id, login}).

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

from_db(Fields) ->
    Map = #{
        id => #user.id,
        login => #user.login
    },
    Ids = [maps:get(F, Map) || F <- Fields],
    fun(Values) ->
        lists:foldl(
            fun({I,V}, U) ->
                erlang:setelement(I, U, V)
            end, #user{}, lists:zip(Ids, Values))
    end.

to_db(#user{id = Id, login = Login}) ->
    #{id => Id, login => Login}.
