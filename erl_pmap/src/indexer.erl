-module(indexer).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start_link/0, index/3]).

start_link() -> gen_server:start_link(?MODULE, [], []).
index(Pid, Url, Contents) -> gen_server:call(Pid, {index, Url, Contents}).	

init([]) -> {ok, []}.

%%% blocking methods

%%% apply standardize, stem, ngram, etc for indexing
handle_call({index, _Url, _Contents}, _From, State) -> {reply, {ok, indexed}, State};

%%% unknown method
handle_call(_Message, _From, State) -> {reply, error, State}.

%%% non-blocking/asynchronous methods
handle_cast(_Message, State) -> {noreply, State}.
handle_info(_Message, State) -> {noreply, State}.

%%% other callback methods for OTP gen-server
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
