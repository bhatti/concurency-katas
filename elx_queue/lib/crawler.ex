defmodule Crawler do
  @max_depth 4

  @moduledoc """
  Documentation for Crawler.
  """

  ## Client API
  def start_link(size) when is_integer(size) do
    {:ok, downloader_pid} = Downloader.start_link()
    {:ok, indexer_pid} = Indexer.start_link()
    GenServer.start_link(__MODULE__, {size, downloader_pid, indexer_pid})
  end

  ## crawl list of url
  def crawl_urls(pid, urls, timeout) when is_pid(pid) and is_list(urls) and is_integer(timeout) do
    crawl_urls(pid, urls, 0, self(), timeout)
  end

  # returns number of urls crawled
  def total_crawl_urls(pid, timeout) when is_pid(pid) do
    GenServer.call(pid, {:total_crawl_urls}, timeout)
  end

  ## internal api to crawl urls
  def crawl_urls(pid, urls, depth, clientPid, timeout) when is_pid(pid) and is_list(urls) and is_pid(clientPid) and is_integer(timeout) do
    if depth < @max_depth do
      requests = urls |> Enum.map(&(Request.new(&1, depth, clientPid, timeout)))
      requests |> Enum.map(&(GenServer.cast(pid, {:crawl, &1})))
    else
      :max_depth_exceeded
    end
  end

  ## dequeue returns pops top request from the queue and returns it
  def dequeue(pid) when is_pid(pid) do
    GenServer.call(pid, {:dequeue})
  end

  ################### 
  ## init method create pool of workers based on given size
  def init({size, downloader_pid, indexer_pid}) when is_integer(size) and is_pid(downloader_pid) and is_pid(indexer_pid) do
    Process.flag(:trap_exit, true)
    pid_to_workers = 0..size |> Enum.map(&child_spec/1)
    |> Enum.map(&start_child/1)
    |> Enum.into(%{})
    {:ok, {pid_to_workers, :queue.new, 0, 0, downloader_pid, indexer_pid}}
  end

  ## asynchronous server handler for adding request to crawl in the queue
  def handle_cast({:crawl, request}, {pid_to_workers, queue, total_in, total_out, downloader_pid, indexer_pid}) do
    new_queue = :queue.in(request, queue)
    {:noreply, {pid_to_workers, new_queue, total_in+1, total_out, downloader_pid, indexer_pid}}
  end

  ## synchronous server handler for returning total urls crawled
  def handle_call({:total_crawl_urls}, _from, {_, _, _total_in, total_out, _, _} = state) do
    {:reply, total_out, state}
  end

  ## synchronous server handler to pop top request from the queue and returning it
  def handle_call({:dequeue}, _from, {pid_to_workers, queue, total_in, total_out, downloader_pid, indexer_pid}) do
    {head, new_queue} = :queue.out(queue)
    if head == :empty do
      {:reply, {head, downloader_pid, indexer_pid}, {pid_to_workers, new_queue, total_in, total_out, downloader_pid, indexer_pid}}
    else
      if rem(:queue.len(queue), 1000) == 0 or rem(total_out+1, 1000) == 0do
        IO.puts("#{total_out+1}...")
      end
      {:value, req} = head
      {:reply, {req, downloader_pid, indexer_pid}, {pid_to_workers, new_queue, total_in, total_out+1, downloader_pid, indexer_pid}}
    end
  end

  ## OTP helper callbacks
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
