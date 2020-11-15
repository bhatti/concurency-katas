%%%-------------------------------------------------------------------
%% @doc erlcrawler top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(erlcrawler_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

-define(MAX_WORKERS, 100).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 0,
                 period => 1,
                 max_restarts => 10,
                 max_seconds => 300
                },
    ChildSpecs = [
                  #{
                    id => erlcrawler_serv,
                    start => {erlcrawler_serv, start_link, []}
                   },
                  #{
                    id => erlcrawler_worker_sup,
                    start => {erlcrawler_worker_sup, start_link, []}
                   }
                 ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
