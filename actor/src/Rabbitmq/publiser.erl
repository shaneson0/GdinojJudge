%%%-------------------------------------------------------------------
%%% @author csx
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. 一月 2017 下午5:09
%%%-------------------------------------------------------------------
-module(publiser).
-author("csx").

%% API
-export([publish/1]).

-include_lib("amqp_client/include/amqp_client.hrl").
-include("task.hrl").

publish(Task) ->
  Channel = rabbitmq_factory:get_channel() ,
  amqp_channel:call(Channel, #'queue.declare'{queue = <<"task_queue">>,
    durable = true}),
  Message = erlang:term_to_binary(Task) ,
  amqp_channel:cast(Channel,
    #'basic.publish'{
      exchange = <<"">>,
      routing_key = <<"task_queue">>},
    #amqp_msg{props = #'P_basic'{delivery_mode = 2},
      payload = Message}),
  io:format(" [x] Sent ~p~n", [Message]),
  ok.

