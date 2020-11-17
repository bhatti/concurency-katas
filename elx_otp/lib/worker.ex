defmodule Worker do
  @moduledoc """
  Documentation for crawling worker.
  """
  @max_url 11
  @domains ["ab.com", "bc.com", "cd.com", "de.com", "ef.com", "fg.com", "gh.com", "hi.com", "ij.com", "jk.com", "kl.com", "lm.com", "mn.com",
        "no.com", "op.com", "pq.com", "qr.com", "rs.com", "st.com", "tu.com", "uv.com", "vw.com", "wx.com", "xy.com", "yz.com"]
  @allowed_chars "abcdefghijklmnopqrstuvwxyz"

  use GenServer

  # Client APIs
  def start_link(crawler_pid) when is_pid(crawler_pid) do
    {:ok, downloader_pid} = Downloader.start_link()
    {:ok, indexer_pid} = Indexer.start_link()
    GenServer.start_link(__MODULE__, {crawler_pid, downloader_pid, indexer_pid})
  end

  @doc """
  Crawls web url asynchronously
  """
  def handle_cast({:crawl, request}, {crawler_pid, downloader_pid, indexer_pid}=state) do
    handle_crawl(crawler_pid, downloader_pid, indexer_pid, request)
    {:noreply, state}
  end

  def init(crawler_pid) do
      {:ok, crawler_pid}
  end

  # Internal private methods
  defp handle_crawl(crawler_pid, downloader_pid, indexer_pid, req) do
    res = Result.new(req)
    contents = Downloader.download(downloader_pid, req.url)
    new_contents = Downloader.jsrender(downloader_pid, req.url, contents)
    if has_content_changed(req.url, new_contents) and !is_spam(req.url, new_contents) do
      Indexer.index(indexer_pid, req.url, new_contents)
      urls = parse_urls(req.url, new_contents)
      Crawler.crawl_urls(crawler_pid, urls, req.depth+1, req.clientPid)
      send req.clientPid, {:crawl_done, Result.completed(res)}
    else
      send req.clientPid, {:crawl_done, Result.failed(req, :skipped_crawl)}
    end
  end

  defp parse_urls(_Url, _Contents) do
    # tokenize contents and extract href/image/script urls
    random_urls(@max_url)
  end

  defp random_urls(n) do
    1..n |> Enum.map(&(random_url/1))
  end

  defp has_content_changed(_url, _contents) do
    # calculate hash digest and compare it with last digest
    true
  end

  defp is_spam(_url, _contents) do
    # apply standardize, stem, ngram, etc for indexing
    false
  end

  defp random_url(_) do
    "https://" <> random_domain() <> "/" <> random_string(20)
  end

  defp random_domain() do
    Enum.random(@domains)
  end

  defp random_string(n) do
    1..n
    |> Enum.reduce([], fn(_, acc) -> [Enum.random(to_charlist(@allowed_chars)) | acc] end)
    |> Enum.join("")
  end
end

