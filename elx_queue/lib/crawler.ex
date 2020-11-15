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

  def dequeue(pid) when is_pid(pid) do
    GenServer.call(pid, {:dequeue})
  end

  ## init method create pool of workers based on given size
  def init(size) when is_integer(size) do
    Process.flag(:trap_exit, true)
    pid_to_workers = 0..size |> Enum.map(&child_spec/1)
    |> Enum.map(&start_child/1)
    |> Enum.into(%{})
    {:ok, {pid_to_workers, :queue.new, 0, 0}}
  end

  ## handler for crawling request
  def handle_cast({:crawl, request}, {pid_to_workers, queue, total_in, total_out}) do
    new_queue = :queue.in(request, queue)
    {:noreply, {pid_to_workers, new_queue, total_in+1, total_out}}
  end

  def handle_call({:total_crawl_urls}, _from, {_, _, _total_in, total_out} = state) do
    {:reply, total_out, state}
  end

  def handle_call({:dequeue}, _from, {pid_to_workers, queue, total_in, total_out}) do
    {head, new_queue} = :queue.out(queue)
    if head == :empty do
      {:reply, head, {pid_to_workers, new_queue, total_in, total_out}}
    else
      if rem(:queue.len(queue), 1000) == 0 or rem(total_out+1, 1000) == 0do
        IO.puts("#{total_out+1}...")
      end
      {:value, req} = head
      {:reply, req, {pid_to_workers, new_queue, total_in, total_out+1}}
    end
  end

  ## OTP Callbacks
  def handle_info({:EXIT, dead_pid, _reason}, {pid_to_workers, queue, total_in, total_out}) do
    # Start new process based on dead_pid spec
    {new_pid, child_spec} = pid_to_workers
    |> Map.get(dead_pid)
    |> start_child()

    # Remove the dead_pid and insert the new_pid with its spec
    new_pid_to_workers = pid_to_workers
    |> Map.delete(dead_pid)
    |> Map.put(new_pid, child_spec)

    {:noreply, {new_pid_to_workers, queue, total_in, total_out}}
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



