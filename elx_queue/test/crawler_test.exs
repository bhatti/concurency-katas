defmodule CrawlerTest do
  use ExUnit.Case
  doctest Crawler
  @root_urls ["a.com", "b.com", "c.com", "d.com", "e.com", "f.com", "g.com", "h.com", "i.com", "j.com", "k.com", "l.com", "n.com"]
  @max_processes 10000
  @max_wait_messages 19032

  test "crawling urls" do
    started = System.system_time(:millisecond)
    {:ok, pid} = Crawler.start_link(@max_processes)
    Crawler.crawl_urls(pid, @root_urls)
    wait_until_total_crawl_urls(pid, @max_wait_messages, started)
  end

  defp wait_until_total_crawl_urls(pid, max, started) do
    receive do {:waiting} -> wait_until_total_crawl_urls(pid, max, started)
    after 100 ->
      n = Crawler.total_crawl_urls(pid)
      if n >= max do
        elapsed = System.system_time(:millisecond) - started
        IO.puts("Crawled URLs in millis: #{n} #{elapsed}")
        assert n >= @max_wait_messages
      else
        wait_until_total_crawl_urls(pid, max, started)
      end
    end
  end

end
