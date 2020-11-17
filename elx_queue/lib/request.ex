defmodule Request do
  @enforce_keys [:clientPid, :url, :depth, :timeout, :created_at]
  defstruct [:clientPid, :url, :depth, :timeout, :created_at]

  def new(url, depth, clientPid, timeout) when is_binary(url) and is_integer(depth) and is_pid(clientPid) and is_integer(timeout) do
    %Request{clientPid: clientPid, url: url, depth: depth, timeout: timeout, created_at: System.system_time(:millisecond)}
  end

end
