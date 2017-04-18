%%%-------------------------------------------------------------------
%%% @author csx
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. 四月 2017 上午9:57
%%%-------------------------------------------------------------------
-module(comsumer).
-author("csx").

-include_lib("amqp_client/include/amqp_client.hrl").
-include("task.hrl").

%% API
-export([comsume/0]).


comsume() ->
  Channel = rabbitmq_factory:get_channel(),
  amqp_channel:call(Channel, #'basic.qos'{prefetch_count = 1}),
  amqp_channel:subscribe(Channel, #'basic.consume'{queue = <<"task_queue">>}, self()),
  receive
    #'basic.consume_ok'{} -> ok
  end,

  io:format("in loop .. ~n"),
  loop(Channel).

start_judge(_Task) ->
  io:format("prepare judge ..~n"),
  ok.

loop(Channel) ->
  receive
    {#'basic.deliver'{delivery_tag = Tag}, #amqp_msg{payload = Body}} ->
      io:format(" [x] Done~n"),
      amqp_channel:cast(Channel, #'basic.ack'{delivery_tag = Tag}),
      Task = erlang:binary_to_term(Body),
      io:format(" [x] Received ~p~n", [Task]),
      start_judge(Task),
      loop(Channel)
  end.