%%%-------------------------------------------------------------------
%% @doc judgenetwork public API
%% @end
%%%-------------------------------------------------------------------

-module(judgenetwork_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    judgenetwork_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================