%%%-------------------------------------------------------------------
%%% @author csx
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. 二月 2017 下午5:49
%%%-------------------------------------------------------------------
-module(network).
-author("csx").

%% API
-export([start/0]).

start() ->
  ok = application:start(network).

