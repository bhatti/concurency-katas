defmodule Downloader do
  use GenServer
  @allowed_chars "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"

  # -------------#
  # Client - API #
  # -------------#

  @moduledoc """
  Downloader -  download cotent
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
  def download(server_pid, url) do
    GenServer.call(server_pid, {:download, url})
  end

  @doc """
  renders dynamic content using javascript
  """
  def jsrender(server_pid, url, contents) do
    GenServer.call(server_pid, {:jsrender, url, contents})
  end

  ##---------- ##
  #Server - API #
  ##-----------##

  def init() do 
    {:ok, []}
  end

  # TODO check robots.txt and throttle policies
  # TODO add timeout for slow websites and linearize requests to the same domain to prevent denial of service attack
  def handle_call({:download, _url}, _from, state) do
    {:reply, {:ok, gen_contents(100)}, state}
  end

  # TODO for SPA apps that use javascript for rendering contents
  def handle_call({:jsrender, _url, _contents}, _from, state) do
    {:reply, {:ok, gen_contents(200)}, state}
  end

  defp gen_contents(n) do
    1..n
    |> Enum.reduce([], fn(_, acc) -> [Enum.random(to_charlist(@allowed_chars)) | acc] end)
    |> Enum.join("")
  end
end
