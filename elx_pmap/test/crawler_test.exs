defmodule CrawlerTest do
  use ExUnit.Case
  doctest Crawler
  @root_urls ["a.com", "b.com", "c.com", "d.com", "e.com", "f.com", "g.com", "h.com", "i.com", "j.com", "k.com", "l.com", "n.com"]
  @expected_urls 19032

  test "crawling urls" do
    timeout = 5 * 60 * 1000
    started = System.system_time(:millisecond)
    res = Crawler.crawl_urls(@root_urls, timeout)
    sum = Enum.reduce(res, 0, &(&1 + &2))
    elapsed = System.system_time(:millisecond) - started
    IO.puts("Crawled URLs in millis: #{sum} #{elapsed}")
    assert sum >= @expected_urls
  end

end
