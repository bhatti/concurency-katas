defmodule Request do
  @enforce_keys [:clientPid, :url, :depth, :created_at]
  defstruct [:clientPid, :url, :depth, :created_at]

  def new(url, depth, clientPid) when is_binary(url) and is_integer(depth) and is_pid(clientPid) do
    %Request{clientPid: clientPid, url: url, depth: depth, created_at: System.system_time(:millisecond)}
  end

end
