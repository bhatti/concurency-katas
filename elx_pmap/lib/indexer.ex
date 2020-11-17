defmodule Indexer do
  use GenServer

  # -------------#
  # Client - API #
  # -------------#

  @moduledoc """
  Indexer -  index cotent
  """
  @doc """
  Start the OTP process
  """
  def start_link() do
    GenServer.start_link(__MODULE__, [], [])
  end
  def init(args) do
    {:ok, args}
  end

  @doc """
  Download contents
  """
  def index(server_pid, url, contents, timeout) do
    GenServer.call(server_pid, {:index, url, contents}, timeout)
  end

  ##---------- ##
  #Server - API #
  ##-----------##

  def init() do 
    {:ok, []}
  end

  # TODO apply standardize, stem, ngram, etc for indexing
  def handle_call({:index, _url, _contents}, _from, state) do
    {:reply, {:ok, :indexed}, state}
  end
end
