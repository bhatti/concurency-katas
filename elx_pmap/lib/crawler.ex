defmodule Crawler do
  @domains ["ab.com", "bc.com", "cd.com", "de.com", "ef.com", "fg.com", "gh.com", "hi.com", "ij.com", "jk.com", "kl.com", "lm.com", "mn.com",
        "no.com", "op.com", "pq.com", "qr.com", "rs.com", "st.com", "tu.com", "uv.com", "vw.com", "wx.com", "xy.com", "yz.com"]
  @allowed_chars "abcdefghijklmnopqrstuvwxyz"
  @max_depth 4
  @max_url 11
  @timeout 1200005

  @moduledoc """
  Documentation for Crawler.
  """

  ## Client API
  def crawl_urls(urls) when is_list(urls) do
    crawl_urls(urls, 0)
  end

  def crawl_urls(urls, depth) when is_list(urls) do
    if depth < @max_depth do
      requests = urls |> Enum.map(&(Request.new(&1, depth)))
      Parallel.pmap(requests, &(handle_crawl/1), @timeout)
    else
      []
    end
  end

  # Internal private methods
  def handle_crawl(req) do
    contents = download(req.url)
    new_contents = jsrender(req.url, contents)
    if has_content_changed(req.url, new_contents) and !is_spam(req.url, new_contents) do
      index(req.url, new_contents)
	  urls = parse_urls(req.url, new_contents)
      res = Crawler.crawl_urls(urls, req.depth+1)
      Enum.reduce(res, 0, &(&1 + &2)) + 1
    else
      0
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
