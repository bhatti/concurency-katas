%%%-------------------------------------------------------------------
%% @doc example stats process
%% @end
%%%-------------------------------------------------------------------

-module(erlcrawler_stats).

-export([start_link/0, dump_stats/0, get_stats/0, terminate/0, succeeded_crawl/0, failed_crawl/0]).

-record(stats, {total_succeeded=0, total_errors=0}).

%%% Client API
start_link() ->
    Pid = spawn_link(fun init/0),
    register(stats_server, Pid),
    Pid.

dump_stats() ->
    Stats = get_stats(),
    io:format("Stats succeeded/failed: ~p ~n", [{Stats}]),
    ok.

get_stats() ->
    Self = self(),
    stats_server ! {get_stats, Self},
    receive {stats_reply, Self, Stats} ->
        Stats
    end.

terminate() ->
    stats_server ! terminate.

succeeded_crawl() ->
    Self = self(),
    stats_server ! {succeeded_crawl, Self},
    receive {succeeded_crawl_reply, Self} ->
        ok
    end.

failed_crawl() ->
    Self = self(),
    stats_server ! {failed_crawl, Self},
    receive {failed_crawl_reply, Self} ->
        ok
    end.

%%% Server functions
init() -> loop(#stats{}).

%%% Main server loop
loop(Stats) ->
    receive
        {succeeded_crawl, Pid} ->
            Pid ! {succeeded_crawl_reply, Pid},
            loop(#stats{total_succeeded = Stats#stats.total_succeeded + 1});
        {failed_crawl, Pid} ->
            Pid ! {failed_crawl_reply, Pid},
            loop(#stats{total_errors = Stats#stats.total_errors + 1});
        {get_stats, Pid} ->
            Pid ! {stats_reply, Pid, Stats},
            loop(Stats);
        {terminate} ->
            ok
    end.

