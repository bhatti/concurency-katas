%%%-------------------------------------------------------------------
%% @doc erlcrawler fake implmenetation of web crawler using processes
%% @end
%%%-------------------------------------------------------------------

-module(erlcrawler).

-export([start_link/0, crawl_urls/3, total_crawl_urls/1]).

-record(request, {clientPid, ref, url, depth, timeout, created_at=erlang:system_time(millisecond)}).
-record(result, {url, status=pending, child_urls=0, started_at=erlang:system_time(millisecond), completed_at, error}).

-define(MAX_DEPTH, 4).
-define(MAX_URL, 11).
-define(DOMAINS, ["ab.com", "bc.com", "cd.com", "de.com", "ef.com", "fg.com", "gh.com", "hi.com", "ij.com", "jk.com", "kl.com", "lm.com", "mn.com",
        "no.com", "op.com", "pq.com", "qr.com", "rs.com", "st.com", "tu.com", "uv.com", "vw.com", "wx.com", "xy.com", "yz.com"]).

make_request(ClientPid, Ref, Url, Depth, Timeout) ->
    #request{clientPid=ClientPid, ref=Ref, url=Url, depth=Depth, timeout=Timeout}.

make_result(Req) ->
    Url = Req#request.url,
    #result{url=Url}.

%%% Client API
start_link() ->
    spawn_link(fun init/0).

%%%%%%%%%%%% public method for crawling %%%%%%%%%%%% 
%%% calling private method for crawling
%%% Pid - process-id of actor
%%% 0 - current depth
%%% Urls - list of urls to crawl
%%% Timeout - max timeout
crawl_urls(Pid, Urls, Timeout) when is_pid(Pid), is_list(Urls)  ->
    do_crawl_urls(Pid, 0, Urls, [], Timeout, 0).

total_crawl_urls(Pid) when is_pid(Pid) ->
    Self = self(),
    Pid ! {total, Self},
    receive {total_reply, Self, N} ->
        N
    end.

%%% Server functions
init() -> 
    {ok, DownloaderPid} = downloader:start_link(),
    {ok, IndexerPid} = indexer:start_link(),
    loop(DownloaderPid, IndexerPid, 0).

%%% Main server loop
loop(DownloaderPid, IndexerPid, N) ->
    receive
        {crawl, Req} ->
            CrawlerPid = self(),
            spawn_link(fun() -> handle_crawl(CrawlerPid, Req, DownloaderPid, IndexerPid) end),
            debug_print(N),
            loop(DownloaderPid, IndexerPid, N+1);
        {total, Pid} ->
            Pid ! {total_reply, Pid, N},
            loop(DownloaderPid, IndexerPid, N);
        terminate ->
            ok
    end.


%%% Internal client functions
debug_print(N) when N rem 10000 == 0 ->
    io:format("~p...~n", [{N}]);
debug_print(_) ->
    ok.

%% Go through URLs to crawl, send asynchronous request to crawl and
%% then add request to a list to monitor that will be used to receive
%% reply back from the crawling actor.
do_crawl_urls(_, _, [], [], _, ChildURLs) ->
    ChildURLs; % all done
do_crawl_urls(_, ?MAX_DEPTH, _, _, _, _) ->
    0; % reached max depth, stop more crawling
do_crawl_urls(Pid, Depth, [Url|T], SubmittedRequests, Timeout, 0) when is_pid(Pid), is_integer(Depth), is_integer(Timeout) ->
    %%% monitoring actor so that we are notified when actor process dies
    Ref = erlang:monitor(process, Pid),
    %%% crawling next url to process
    Req = make_request(self(), Ref, Url, Depth, Timeout),
    Pid ! {crawl, Req},
    do_crawl_urls(Pid, Depth, T, SubmittedRequests ++ [Req], Timeout, 0);
do_crawl_urls(Pid, Depth, [], [Req|T], Timeout, ChildURLs) when is_pid(Pid) ->
    %%% receiving response from the requests that were previously stored
    Ref = Req#request.ref,
    receive
        {crawl_done, Ref, Res} ->
            erlang:demonitor(Ref, [flush]),
            do_crawl_urls(Pid, Depth, [], T, Timeout, Res#result.child_urls+ChildURLs+1);
        {'DOWN', Ref, process, Pid, Reason} ->
            erlang:error(Reason)
    after Timeout ->
        erlang:error({crawl_timeout, Timeout})
    end.


%%% Internal server functions called by actor to process the crawling request
handle_crawl(CrawlerPid, Req, DownloaderPid, IndexerPid) ->
    Res = make_result(Req),
    ClientPid = Req#request.clientPid,
    Url = Req#request.url,
    Ref = Req#request.ref,
    Depth = Req#request.depth,
    Timeout = Req#request.timeout,

    case downloader:download(DownloaderPid, Url) of
        {ok, Contents} ->
        {ok, Contents1} = downloader:jsrender(DownloaderPid, Url, Contents),
        Changed = has_content_changed(Url, Contents1),
        Spam = is_spam(Url, Contents1),
        if Changed and not Spam ->
            indexer:index(IndexerPid, Url, Contents1),
        Urls = parse_urls(Url, Contents1),
                %% Crawling child urls synchronously before returning
                ChildURLs = do_crawl_urls(CrawlerPid, Depth+1, Urls, [], Timeout, 0) + 1,
                Res1 = Res#result{completed_at=erlang:system_time(millisecond), child_urls=ChildURLs},
                ClientPid ! {crawl_done, Ref, Res1};
            true ->
                Res1 = Res#result{completed_at=erlang:system_time(millisecond)},
                ClientPid ! {crawl_done, Ref, Res1}
            end;
        Err ->
            Res1 = Res#result{completed_at=erlang:system_time(millisecond), error = Err},
            ClientPid ! {crawl_done, Ref, Res1}
        end,
    ok.

%%%%%%%%%%%%%%% INTERNAL METHODS FOR CRAWLING %%%%%%%%%%%%%%%%
parse_urls(_Url, _Contents) ->
    % tokenize contents and extract href/image/script urls
    random_urls(?MAX_URL).

random_urls(N) ->
    [random_url() || _ <- lists:seq(1, N)].

has_content_changed(_Url, _Contents) ->
     % calculate hash digest and compare it with last digest
    true.

is_spam(_Url, _Contents) ->
     % apply standardize, stem, ngram, etc for indexing
    false.

random_url() ->
    "https://" ++ random_domain() ++ "/" ++ random_string(20).

random_domain() ->
    lists:nth(random:uniform(length(?DOMAINS)), ?DOMAINS).

random_string(Length) ->
    AllowedChars = "abcdefghijklmnopqrstuvwxyz",
    lists:foldl(fun(_, Acc) -> [lists:nth(random:uniform(length(AllowedChars)), AllowedChars)] ++ Acc end, [], lists:seq(1, Length)).
