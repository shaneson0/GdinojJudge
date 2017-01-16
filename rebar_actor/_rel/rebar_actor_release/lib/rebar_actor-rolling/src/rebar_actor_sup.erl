%%%-------------------------------------------------------------------
%%% @author csx
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. 一月 2017 上午8:05
%%%-------------------------------------------------------------------
-module(rebar_actor_sup).
-author("csx").

-behaviour(supervisor).

%% API
-export([start_link/0,init/1]).

-spec start_link() -> {ok, pid() }.
start_link() ->
  supervisor:start_link({local,?MODULE}, ?MODULE , [] ).

init([]) ->
  Procs = [],
  {ok , {{one_for_one ,10,10} , Procs}}.
