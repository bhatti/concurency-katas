##--------------------------------------------------------------------------
# tests for fake web crawler using otp
##--------------------------------------------------------------------------

defmodule CrawlerTest do
  use ExUnit.Case
  doctest Crawler
  @max_processes 10000
  @max_wait_messages 19032
  @root_urls ["a.com", "b.com", "c.com", "d.com", "e.com", "f.com", "g.com", "h.com", "i.com", "j.com", "k.com", "l.com", "n.com"]

  test "test crawling urls" do
    started = System.system_time(:millisecond)
    {:ok, pid} = Crawler.start_link(@max_processes)
    Crawler.crawl_urls(pid, @root_urls)
    wait_until_total_crawl_urls(pid, @max_wait_messages, started)
  end

  defp wait_until_total_crawl_urls(pid, 0, started) do
    n = Crawler.total_crawl_urls(pid)
    elapsed = System.system_time(:millisecond) - started
    IO.puts("Crawled URLs in millis: #{n} #{elapsed}")
    assert n >= @max_wait_messages
  end

  defp wait_until_total_crawl_urls(pid, max, started) do
    if rem(max, 1000) == 0 do
      IO.puts("#{max}...")
    end
    receive do
      {:crawl_done, _} -> wait_until_total_crawl_urls(pid, max-1, started)
    end
  end

end
