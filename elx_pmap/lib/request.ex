defmodule Request do
  @enforce_keys [:url, :depth, :created_at]
  defstruct [:url, :depth, :created_at]

  def new(url, depth) when is_binary(url) and is_integer(depth) do 
    %Request{url: url, depth: depth, created_at: System.system_time(:millisecond)}
  end

end
