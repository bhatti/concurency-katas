%%%-------------------------------------------------------------------
%% @doc erlcrawler public API
%% @end
%%%-------------------------------------------------------------------

-module(erlcrawler_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    erlcrawler_sup_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
