%%%-------------------------------------------------------------------
%%% @author csx
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. 四月 2017 上午8:34
%%%-------------------------------------------------------------------
-module(rabbitmq_factory).
-author("csx").

-behaviour(gen_server).
-include_lib("amqp_client/include/amqp_client.hrl").

%% API
-export([start_link/0,get_channel/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {channel,connect}).

%%%===================================================================
%%% API
%%%===================================================================

get_channel() ->
  Channel = gen_server:call( ?MODULE , {getchannel} ),
  Channel.

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  io:format("start client server~n"),
  {ok, Connection} =
    amqp_connection:start(#amqp_params_network{host = "localhost"}),
  {ok,Channel} = amqp_connection:open_channel(Connection),
  amqp_channel:call(Channel, #'queue.declare'{queue = <<"task_queue">>,
    durable = true}),
  {ok, #state{channel = Channel,connect = Connection}}.

handle_call({getchannel},_From,State) ->
  Channel = State#state.channel,
  {reply,Channel,State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.
handle_cast(_Request, State) ->
  {noreply, State}.
handle_info(_Info, State) ->
  {noreply, State}.
terminate(_Reason, _State) ->
  ok.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


