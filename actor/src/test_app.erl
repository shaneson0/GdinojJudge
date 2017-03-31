%%%-------------------------------------------------------------------
%% @doc test public API
%% @end
%%%-------------------------------------------------------------------

-module(test_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->

    %%cowboy编译，设置路由
    Dispatch = cowboy_router:compile([
      {'_', [
        {"/gdinoj/judge/", test_handler, []}
      ]}
    ]),

    %%启动http服务
    {ok, _} = cowboy:start_http(http, 100, [{port, 8086}], [
      {env, [{dispatch, Dispatch}]}
    ]),

    %% 启动rabbitm_server
    rabbitmq_factory:start(),

    test_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
