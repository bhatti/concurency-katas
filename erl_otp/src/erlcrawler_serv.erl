%%%-------------------------------------------------------------------
%% @doc erlcrawler main gen-server
%% @end
%%%-------------------------------------------------------------------

-module(erlcrawler_serv).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1]).

-record(request, {url, status=pending, created_at=erlang:system_time(millisecond), started_at, completed_at, error}).
-record(stats, {started_at=erlang:system_time(millisecond), completed_at, total_urls_crawled, total_errors}).

-define(MAX_DEPTH, 100).

make_request(Url) ->
#request{url=Url}.

make_stats() ->
#stats{}.

start_link() ->
    gen_server:start_link(?MODULE, [], []).


init([]) ->
    io:format("erlcrawler_serv Web Crawler!~n"),
    %halt(0). % shut down the VM without error
    {ok, []}.

%% internal functions
