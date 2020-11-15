%%%-------------------------------------------------------------------
%% @doc tests for erlcrawler that uses pmap
%% @end
%%%-------------------------------------------------------------------

-module(erlcrawler_test).
-export([square/1, error_square/1]).

-include_lib("eunit/include/eunit.hrl").

-define(ROOT_URLS, ["a.com", "b.com", "c.com", "d.com", "e.com", "f.com", "g.com", "h.com", "i.com", "j.com", "k.com", "l.com", "n.com"]).

pmap_square_test() ->
    [{ok, 1}, {ok, 4}, {ok, 9}] = pmap:pmap(fun erlcrawler_test:square/1, [1, 2, 3], 100).

pmap_square_error_test() ->
    [{error, _}, {error, _}, {error, _}] = pmap:pmap(fun erlcrawler_test:error_square/1, [1, 2, 3], 100).

square(N) -> N * N.

error_square(N) -> erlong:error({failed_square}).


%%% Test Client API
crawl_urls_test() ->
    {timeout, 5000, do_crawl_urls(5000)}.


%% Testing timeout and by default, it will terminate the test process so we will instead convert
%% kill signal into a message using erlang:exit
crawl_urls_with_timeout_test() ->
    %%% crawling next url to process
    Started = erlang:system_time(millisecond),
    Timeout = 10, % We know that processing takes longer than 10 milliseconds
    process_flag(trap_exit, true),
    spawn_link(fun() ->
        erlcrawler:crawl_urls(?ROOT_URLS, Timeout)
    end),
    %{{pmap_timeout, _}, _} = receive
    {{pmap_timeout,10}, _} = receive
        {'EXIT', _, Reason} -> Reason
    after 200 ->
        erlang:error(unexpected_timeout)
    end,
    Elapsed = erlang:system_time(millisecond) - Started,
    ?debugFmt("crawl_urls_with_timeout_test: timedout as expected in millis ~p ~n", [{Elapsed}]).

%% Testing terminate/cancellation and killing a process will kill all its children
crawl_urls_with_terminate_test() ->
    %%% crawling next url to process
    Started = erlang:system_time(millisecond),
    process_flag(trap_exit, true),
    Child = spawn_link(fun() ->
        erlcrawler:crawl_urls(?ROOT_URLS, 1000) % crawl_urls is synchronous method so calling in another process
    end),
    receive
    after 15 -> % waiting for a bit before terminating (canceling) process
        exit(Child, {test_terminated})
    end,
    {test_terminated} = receive
        {'EXIT', Child, Reason} -> Reason
    after 200 ->
        erlang:error(unexpected_timeout)
    end,
    Elapsed = erlang:system_time(millisecond) - Started,
    ?debugFmt("crawl_urls_with_terminate_test: terminated as expected in millis ~p ~n", [{Elapsed}]).

do_crawl_urls(Timeout) ->
    Started = erlang:system_time(millisecond),
    Res = erlcrawler:crawl_urls(?ROOT_URLS, Timeout),
    Size = erlcrawler:ok_size(Res, 0),
    Elapsed = erlang:system_time(millisecond) - Started,
    ?debugFmt("do_crawl_urls: Crawled URLs in millis: ~p ~n", [{Size, Elapsed}]),
    ?assertEqual(Size, 19032).
