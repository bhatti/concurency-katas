%%%-------------------------------------------------------------------
%% @doc erlcrawler implmenetation using pmap
%% @end
%%%-------------------------------------------------------------------

-module(erlcrawler).

-export([crawl_urls/2, handle_crawl/1, ok_size/2]).

-record(request, {url, depth, timeout, created_at=erlang:system_time(millisecond)}).
-record(result, {url, status=pending, child_urls=0, started_at=erlang:system_time(millisecond), completed_at, error}).

-define(MAX_DEPTH, 4).
-define(MAX_URL, 11).
-define(ROOT_URLS, ["a.com", "b.com", "c.com", "d.com", "e.com", "f.com", "g.com", "h.com", "i.com", "j.com", "k.com", "l.com", "n.com"]).
-define(DOMAINS, ["ab.com", "bc.com", "cd.com", "de.com", "ef.com", "fg.com", "gh.com", "hi.com", "ij.com", "jk.com", "kl.com", "lm.com", "mn.com",
        "no.com", "op.com", "pq.com", "qr.com", "rs.com", "st.com", "tu.com", "uv.com", "vw.com", "wx.com", "xy.com", "yz.com"]).

make_request(Url, Depth, Timeout) ->
    #request{url=Url, depth=Depth, timeout=Timeout}.

make_result(Req) ->
    Url = Req#request.url,
    #result{url=Url}.

%%%%%%%%%%%% public method for crawling %%%%%%%%%%%% 
%%% calling private method for crawling
%%% Urls - list of urls to crawl
%%% Timeout - max timeout
crawl_urls(Urls, Timeout) when is_list(Urls), is_integer(Timeout) ->
    crawl_urls(Urls, 0, Timeout).

crawl_urls(_, ?MAX_DEPTH, _) ->
    [];
crawl_urls(Urls, Depth, Timeout) when is_list(Urls), is_integer(Timeout), is_integer(Depth) ->
    Requests = [make_request(R, Depth, Timeout) || R <- Urls],
    pmap:pmap(fun erlcrawler:handle_crawl/1, Requests, Timeout).

%% Crawling a single request and then crawl child URLs using
%% high level crawl_urls, which would use pmap to crawl all URLs
%% in parallel and wait for reply.
%% Note: This method will not return until all child URLs are
%% processed so that it can provide total count or errors to top
%% method.
handle_crawl(Req) ->
    Res = make_result(Req),
    Url = Req#request.url,
    Depth = Req#request.depth,
    Timeout = Req#request.timeout,

	case download(Url) of
        {ok, Contents} ->
	        Contents1 = jsrender(Url, Contents),
	        Changed = has_content_changed(Url, Contents1),
	        Spam = is_spam(Url, Contents1),
	        if Changed and not Spam ->
			    index(Url, Contents1),
			    Urls = parse_urls(Url, contents),
                Res1 = crawl_urls(Urls, Depth+1, Timeout),
                Size = ok_size(Res1, 0),
                Size+1;
            true ->
                1
            end;
        _ ->
            1
        end.

download(_Url) ->
    % TODO check robots.txt and throttle policies
    % TODO add timeout for slow websites and linearize requests to the same domain to prevent denial of service attack
    {ok, random_string(100)}.

jsrender(_Url, _Contents) ->
 	% for SPA apps that use javascript for rendering contents 
    ok.

index(_Url, _Contents) ->
 	% apply standardize, stem, ngram, etc for indexing
    ok.

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

%%% deconstruct size from array of counts such as [{ok, 3}, {ok, 5}...]
ok_size([{ok, N}|T], Sum) ->
    ok_size(T, Sum+N);
ok_size([], Sum) ->
    Sum.