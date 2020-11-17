%%%-------------------------------------------------------------------
%% @doc tests for erlcrawler using actor processes
%% @end
%%%-------------------------------------------------------------------

-module(erlcrawler_test).
-include_lib("eunit/include/eunit.hrl").

-define(ROOT_URLS, ["a.com", "b.com", "c.com", "d.com", "e.com", "f.com", "g.com", "h.com", "i.com", "j.com", "k.com", "l.com", "n.com"]).

crawl_urls_test() ->
    {spawn, {timeout,30, do_crawl_urls(10000)}}.

%% Testing timeout and by default, it will terminate the test process so we will instead convert
%% kill signal into a message using erlang:exit
crawl_urls_with_timeout_test() ->
    %%% crawling next url to process
    Started = erlang:system_time(millisecond),
    Timeout = 10, % We know that processing takes longer than 10 milliseconds
    Pid = erlcrawler:start_link(),
    process_flag(trap_exit, true),
    spawn_link(fun() ->
        erlcrawler:crawl_urls(Pid, ?ROOT_URLS, Timeout)
    end),
    {{crawl_timeout, _}, _} = receive
        {'EXIT', _, Reason} -> Reason
    after 1000 ->
        erlang:error(unexpected_timeout)
    end,
    Elapsed = erlang:system_time(millisecond) - Started,
    ?debugFmt("crawl_urls_with_timeout_test: timedout as expected in millis ~p ~n", [{Elapsed}]).

%% Testing terminate/cancellation and killing a process will kill all its children
crawl_urls_with_terminate_test() ->
    %%% crawling next url to process
    Started = erlang:system_time(millisecond),
    Pid = erlcrawler:start_link(),
    spawn_link(fun() ->
        erlcrawler:crawl_urls(Pid, ?ROOT_URLS, 1000) % crawl_urls is synchronous method so calling in another process
    end),
    receive
    after 15 -> % waiting for a bit before terminating (canceling) process
        exit(Pid, {test_terminated})
    end,
    {test_terminated} = receive
        {'EXIT', Pid, Reason} -> Reason
    after 200 ->
        erlang:error(unexpected_timeout)
    end,
    Elapsed = erlang:system_time(millisecond) - Started,
    ?debugFmt("crawl_urls_with_terminate_test: terminated as expected in millis ~p ~n", [{Elapsed}]).

do_crawl_urls(Timeout) ->
    Started = erlang:system_time(millisecond),
    Pid = erlcrawler:start_link(),
    N = erlcrawler:crawl_urls(Pid, ?ROOT_URLS, Timeout),
    N1 = erlcrawler:total_crawl_urls(Pid),
    Elapsed = erlang:system_time(millisecond) - Started,
    ?debugFmt("do_crawl_urls: Crawled URLs in millis: ~p ~n", [{N, N1, Elapsed}]),
    ?assertEqual(N1, 19032).
