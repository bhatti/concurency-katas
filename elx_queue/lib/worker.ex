defmodule Worker do
  @moduledoc """
  Documentation for crawling worker that dequeues top request from shared queue and work on it. It then sends reply back to client using its pid.
  """
  @max_url 11
  @domains [
    "ab.com",
    "bc.com",
    "cd.com",
    "de.com",
    "ef.com",
    "fg.com",
    "gh.com",
    "hi.com",
    "ij.com",
    "jk.com",
    "kl.com",
    "lm.com",
    "mn.com",
    "no.com",
    "op.com",
    "pq.com",
    "qr.com",
    "rs.com",
    "st.com",
    "tu.com",
    "uv.com",
    "vw.com",
    "wx.com",
    "xy.com",
    "yz.com"]
  @allowed_chars "abcdefghijklmnopqrstuvwxyz"

  # Client APIs
  def start_link(serverPid) when is_pid(serverPid) do
    {:ok, spawn_link(fn -> loop(serverPid) end)}
  end

  # APIs
  @doc """
  crawls url request
  """
  def loop(serverPid) when is_pid(serverPid) do
    {req, downloader_pid, indexer_pid} = Crawler.dequeue(serverPid)
    handle_crawl(serverPid, req, downloader_pid, indexer_pid)
    loop(serverPid)
  end

  # Internal private methods
  defp handle_crawl(_, :empty, _, _) do
    # throttling in case of empty queue -- waiting for 1 millisecond
    receive do {:waiting} -> :queue_empty_error
    after 1 -> :queue_empty_error
    end
  end

  defp handle_crawl(serverPid, req, downloader_pid, indexer_pid) do
    res = Result.new(req)
    {:ok, contents} = Downloader.download(downloader_pid, req.url, req.timeout)
    {:ok, new_contents} = Downloader.jsrender(downloader_pid, req.url, contents, req.timeout)
    if has_content_changed(req.url, new_contents) and !is_spam(req.url, new_contents) do
      Indexer.index(indexer_pid, req.url, new_contents, req.timeout)
      urls = parse_urls(req.url, new_contents)
      Crawler.crawl_urls(serverPid, urls, req.depth+1, req.clientPid, req.timeout)
      #res1 = Result.completed(res)
      #send req.clientPid, {:crawl_done, res1}
    else
      #res1 = Result.failed(res, :skipped_crawl)
      #send req.clientPid, {:crawl_done, res1}
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

