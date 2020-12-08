%%%-------------------------------------------------------------------
%% @doc mnclp public API
%% @end
%%%-------------------------------------------------------------------

-module(mnclp_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    mnclp_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
