%%%-------------------------------------------------------------------
%%% @author csx
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. 二月 2017 上午8:13
%%%-------------------------------------------------------------------
-module(judgenetwork).
-author("csx").

%% API
-export([start/0]).


start() ->
  ok = application:start(judgenetwork).
