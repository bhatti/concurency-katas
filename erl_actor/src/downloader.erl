-module(downloader).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start_link/0, download/2, jsrender/3]).

start_link() -> gen_server:start_link(?MODULE, [], []). % {local, ?MODULE} will register
download(Pid, Url) -> gen_server:call(Pid, {download, Url}). % ?MODULE
jsrender(Pid, Url, Contents) -> gen_server:call(Pid, {jsrender, Url, Contents}).

init([]) -> {ok, []}.

%%% blocking methods

% TODO check robots.txt and throttle policies
% TODO add timeout for slow websites and linearize requests to the same domain to prevent denial of service attack
handle_call({download, Url}, _From, State) -> {reply, {ok, gen_contents(100)}, State};

% for SPA apps that use javascript for rendering contents 
handle_call({jsrender, Url, Contents}, _From, State) -> {reply, {ok, gen_contents(200)}, State};

% unknown methods
handle_call(_Message, _From, State) -> {reply, error, State}.

%%% non-blocking/asynchronous methods
handle_cast(_Message, State) -> {noreply, State}.
handle_info(_Message, State) -> {noreply, State}.

%%% other callback methods for OTP gen-server
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.

%%% helper method to generate contents
gen_contents(Length) ->
    AllowedChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890",
    lists:foldl(fun(_, Acc) -> [lists:nth(random:uniform(length(AllowedChars)), AllowedChars)] ++ Acc end, [], lists:seq(1, Length)).
