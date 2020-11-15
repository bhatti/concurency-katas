%%%-------------------------------------------------------------------
%% @doc erlcrawler top level super supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(erlcrawler_sup_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

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
                    id => erlcrawler_main,
                    start => {erlcrawler_sup, start_link, []}
                   }],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
