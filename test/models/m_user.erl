-module(m_user).
-behaviour(repo_model).

-export([
         schema/0,
         before_save/3,
         after_save/4,

         before_delete/3,

         from_db/1,
         to_db/1
        ]).

-record(user, {id, login, tag = null}).

schema() ->
    #{ fields => #{
           id => #{type => integer, required => true, index => true, readOnly => true},
           login => #{type => {varchar, 255}, required => true},
           tag => #{type => text}
       },
       table => <<"users">>
    }.

before_save(_C, _Model, #{error := Reason}) ->
    {error, Reason};
before_save(_C, Model, _HookOpts) ->
    catch (test_srv ! {'before', Model}),
    {ok, Model}.

before_delete(_C, _Q, #{error := Reason}) ->
    {error, Reason};
before_delete(C, Q, HookOpts) ->
    repo_model:before_delete(C, Q, HookOpts).

after_save(_C, _BeforeModel, AfterModel, _HookOpts) ->
    catch (test_srv ! {'after', AfterModel}),
    AfterModel.

from_db(Fields) ->
    Map = #{
        id => #user.id,
        login => #user.login,
        tag => #user.tag
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
