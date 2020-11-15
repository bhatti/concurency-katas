%%%-------------------------------------------------------------------
%% @doc erlcrawler top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(erlcrawler_worker_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link({M, F, A}) ->
    supervisor:start_link(?MODULE, {M, F, A} ).
%supervisor:start_link({local, ?MODULE}, {M, F, A}, []).


%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init({M, F, A}) ->
    SupFlags = #{
                  strategy  => simple_one_for_one,
                  intensity => 0,
                  period    => 1
                },

    ChildSpec = #{
                   id       => undefined,
                   start    => {M, F, A},
                   restart  => temporary,
                   shutdown => 5000,
                   type     => worker,
                   modules  => [M]
                 },

    {ok, {SupFlags, [ChildSpec]}}.

%% internal functions
start_child() ->
  supervisor:start_child(?MODULE, []).

start_children(N) ->
  [start_child() || _ <- lists:seq(1, N)],
  ok.
