defmodule Crawler do
  @domains ["ab.com", "bc.com", "cd.com", "de.com", "ef.com", "fg.com", "gh.com", "hi.com", "ij.com", "jk.com", "kl.com", "lm.com", "mn.com",
        "no.com", "op.com", "pq.com", "qr.com", "rs.com", "st.com", "tu.com", "uv.com", "vw.com", "wx.com", "xy.com", "yz.com"]
  @allowed_chars "abcdefghijklmnopqrstuvwxyz"
  @max_depth 4
  @max_url 11

  @moduledoc """
  Documentation for Crawler.
  """

  ## Client API
  def crawl_urls(urls, timeout) when is_list(urls) do
    {:ok, downloader_pid} = Downloader.start_link()
    {:ok, indexer_pid} = Indexer.start_link()
    crawl_urls(urls, downloader_pid, indexer_pid, 0, timeout)
  end

  def crawl_urls(urls, downloader_pid, indexer_pid, depth, timeout) when is_list(urls) and is_pid(downloader_pid) and is_pid(indexer_pid) and is_integer(depth) and is_integer(timeout) do
    if depth < @max_depth do
      requests = urls |> Enum.map(&(Request.new(&1, downloader_pid, indexer_pid, depth, timeout)))
      Parallel.pmap(requests, &(handle_crawl/1), timeout)
    else
      []
    end
  end

  # Internal private methods
  defp handle_crawl(req) do
    {:ok, contents} = Downloader.download(req.downloader_pid, req.url, req.timeout)
    {:ok, new_contents} = Downloader.jsrender(req.downloader_pid, req.url, contents, req.timeout)
    if has_content_changed(req.url, new_contents) and !is_spam(req.url, new_contents) do
      Indexer.index(req.indexer_pid, req.url, new_contents, req.timeout)
      urls = parse_urls(req.url, new_contents)
      res = Crawler.crawl_urls(urls, req.downloader_pid, req.indexer_pid, req.depth+1, req.timeout)
      Enum.reduce(res, 0, &(&1 + &2)) + 1
    else
      0
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
