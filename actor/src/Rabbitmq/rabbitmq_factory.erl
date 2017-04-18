%%%-------------------------------------------------------------------
%%% @author csx
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. 一月 2017 下午8:10
%%%-------------------------------------------------------------------
-module(rabbitmq_factory).
-author("csx").

-behaviour(gen_server).


%%include
-include_lib("amqp_client/include/amqp_client.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-export([start/0,get_channel/0]).

-define(SERVER, ?MODULE).

-record(state, {channel,connect}).

%%%===================================================================
%%% API
%%%===================================================================


start() -> start_link().
get_channel() ->
  Channel = gen_server:call( ?MODULE , {getchannel} ),
  io:format("channel : ~p ～n",[Channel]),
  Channel.

start_link() ->
  io:format("start the server") ,
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


init([]) ->
  {ok, Connection} =
    amqp_connection:start(#amqp_params_network{host = "localhost"}),
  {ok, Channel} = amqp_connection:open_channel(Connection),
  amqp_channel:call(Channel, #'queue.declare'{queue = <<"task_queue">>,
    durable = true}),

  {ok, #state{channel = Channel,connect = Connection}}.

handle_call( {getchannel} , _From , State )->
  io:format("handle call getchannel."),
  Channel = State#state.channel ,
  { reply , Channel , State };
handle_call(_Request, _From, State) ->
  {reply, ok, State}.


handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, State) ->
  Connection = State#state.connect ,
  Channel = State#state.channel ,
  ok = amqp_channel:close(Channel),
  ok = amqp_connection:close(Connection),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
