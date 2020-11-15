%%%-------------------------------------------------------------------
%% @doc pmap implementation in erlang
%% @end
%% adopted from https://medium.com/@jlouis666/testing-a-parallel-map-implementation-2d9eab47094e
%%%-------------------------------------------------------------------

-module(pmap).

-export([pmap/3]).

pmap(F, Es, Timeout) ->
   Parent = self(),
   Running = [exec(Parent, F, E) || E <- Es],
   collect(Running, Timeout).

exec(Parent, F, E) ->
    spawn_monitor(fun() -> Parent ! {self(), F(E)} end).

collect([], _Timeout) -> [];
collect([{Pid, MRef} | Next], Timeout) ->
  receive
    {Pid, Res} ->
      erlang:demonitor(MRef, [flush]),
      [{ok, Res} | collect(Next, Timeout)];
    {'DOWN', MRef, process, Pid, Reason} ->
      [{error, Reason} | collect(Next, Timeout)]
  after Timeout ->
    erlang:error({pmap_timeout, Timeout})
  end.
