##--------------------------------------------------------------------------
# fake web crawler using thread pool
##--------------------------------------------------------------------------

defmodule Crawler do
  @max_depth 4

  @moduledoc """
  Documentation for Crawler.
  """

  ## Client API
  # {:ok, pid} = Crawler.start_link(100000)
  def start_link(size) when is_integer(size) do
    GenServer.start_link(__MODULE__, size)
  end

  def total_crawl_urls(pid) when is_pid(pid) do
    GenServer.call(pid, {:total_crawl_urls}, 30000)
  end

  def crawl_urls(pid, urls) when is_pid(pid) and is_list(urls) do
    crawl_urls(pid, urls, 0, self())
  end

  def crawl_urls(pid, urls, depth, clientPid) when is_pid(pid) and is_list(urls) do
    if depth < @max_depth do
      requests = urls |> Enum.map(&(Request.new(&1, depth, clientPid)))
      requests |> Enum.map(&(GenServer.cast(pid, {:crawl, &1})))
    else
      :max_depth_exceeded
    end
  end

  ## init method create pool of workers based on given size
  def init(size) when is_integer(size) do
    Process.flag(:trap_exit, true)
    pid_to_workers = 0..size |> Enum.map(&child_spec/1)
    |> Enum.map(&start_child/1)
    |> Enum.into(%{})
    pids = Map.keys(pid_to_workers)
    {:ok, {pid_to_workers, pids, 0}}
  end

  ## handles crawling
  def handle_cast({:crawl, request}, {pid_to_workers, [pid|rest], total_in}) do
    GenServer.cast(pid, {:crawl, request}) # send request to workers in round-robin fashion
    {:noreply, {pid_to_workers, rest ++ [pid], total_in+1}}
  end

  def handle_call({:total_crawl_urls}, _from, {_, _, total_in} = state) do
    {:reply, total_in, state}
  end

  ## OTP Callbacks
  def handle_info({:EXIT, dead_pid, _reason}, {pid_to_workers, _, total_in}) do
    # Start new process based on dead_pid spec
    {new_pid, child_spec} = pid_to_workers
    |> Map.get(dead_pid)
    |> start_child()

    # Remove the dead_pid and insert the new_pid with its spec
    new_pid_to_workers = pid_to_workers
    |> Map.delete(dead_pid)
    |> Map.put(new_pid, child_spec)
    pids = Map.keys(new_pid_to_workers)
    {:noreply, {new_pid_to_workers, pids, total_in}}
  end

  ## Defines spec for worker
  defp child_spec(_) do
    {Worker, :start_link, [self()]}
  end

  ## Dynamically create child
  defp start_child({module, function, args} = spec) do
    {:ok, pid} = apply(module, function, args)
    Process.link(pid)
    {pid, spec}
  end

end



