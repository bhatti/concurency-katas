%%%-------------------------------------------------------------------
%% @doc erlcrawler worker
%% @end
%%%-------------------------------------------------------------------

-module(erlcrawler_worker).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1]).


start_link() ->
    gen_server:start_link(?MODULE, [], []).


init([]) ->
    io:format("erlcrawler_worker Web Worker!~n"),
    %halt(0). % shut down the VM without error
    {ok, []}.

%% internal functions
