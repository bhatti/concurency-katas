defmodule Request do
  @enforce_keys [:url, :depth, :created_at]
  defstruct [:url, :downloader_pid, :indexer_pid, :depth, :timeout, :created_at]

  def new(url, downloader_pid, indexer_pid, depth, timeout) when is_binary(url) and is_pid(downloader_pid) and is_pid(indexer_pid) and is_integer(depth) and is_integer(timeout) do 
    %Request{url: url, downloader_pid: downloader_pid, indexer_pid: indexer_pid, depth: depth, timeout: timeout, created_at: System.system_time(:millisecond)}
  end

end
