%%%-------------------------------------------------------------------
%%% @author csx
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. 一月 2017 上午7:49
%%%-------------------------------------------------------------------
-module(rebar_actor_app).
-author("csx").



%% API
-export([start/2,stop/0]).


start( _Type , _Args ) ->
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/", actor_handle, []}
    ]}
  ]),
  {ok, _} = cowboy:start_clear(http, 100, [{port, 8080}], #{
    env => #{dispatch => Dispatch}
  }),
  rebar_actor_sup:start_link().

stop() ->
  ok.



