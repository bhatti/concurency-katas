defmodule Worker do
  @moduledoc """
  Documentation for crawling worker.
  """
  @max_url 11
  @domains ["ab.com", "bc.com", "cd.com", "de.com", "ef.com", "fg.com", "gh.com", "hi.com", "ij.com", "jk.com", "kl.com", "lm.com", "mn.com",
        "no.com", "op.com", "pq.com", "qr.com", "rs.com", "st.com", "tu.com", "uv.com", "vw.com", "wx.com", "xy.com", "yz.com"]
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
    req = Crawler.dequeue(serverPid)
    handle_crawl(serverPid, req)
    loop(serverPid)
  end

  # Internal private methods
  defp handle_crawl(_, :empty) do
    # throttling in case of empty queue -- waiting for 1 millisecond
    receive do {:waiting} -> :queue_empty_error
    after 1 -> :queue_empty_error
    end
  end
  defp handle_crawl(serverPid, req) do
    res = Result.new(req)
    contents = download(req.url)
    new_contents = jsrender(req.url, contents)
    if has_content_changed(req.url, new_contents) and !is_spam(req.url, new_contents) do
      index(req.url, new_contents)
	  urls = parse_urls(req.url, new_contents)
      Crawler.crawl_urls(serverPid, urls, req.depth+1, req.clientPid)
      #res1 = Result.completed(res)
      #send req.clientPid, {:crawl_done, res1}
    else
      #res1 = Result.failed(res, :skipped_crawl)
      #send req.clientPid, {:crawl_done, res1}
    end
  end

  defp download(_url) do
    # TODO check robots.txt and throttle policies
    # TODO add timeout for slow websites and linearize requests to the same domain to prevent denial of service attack
    random_string(100)
  end

  defp jsrender(_url, _contents) do
    # for SPA apps that use javascript for rendering contents
    :ok
  end

  defp index(_url, _contents) do
    # apply standardize, stem, ngram, etc for indexing
    :ok
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

