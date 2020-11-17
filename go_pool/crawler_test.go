package crawler

import (
	"context"
	"log"
	"testing"
	"time"
)

const EXPECTED_URLS = 19032

func TestCrawl(t *testing.T) {
	rootUrls := []string{"https://a.com", "https://b.com", "https://c.com", "https://d.com", "https://e.com", "https://f.com", "https://g.com", "https://h.com", "https://i.com", "https://j.com", "https://k.com", "https://l.com", "https://n.com"}
	started := time.Now()
	timeout := time.Duration(8 * time.Second)
	ctx, cancel := context.WithTimeout(context.Background(), timeout)
	defer cancel()
	crawler := New(ctx)
	received, err := crawler.Crawl(ctx, rootUrls, timeout)
	elapsed := time.Since(started)
	log.Printf("Crawl took %s to process %v messages -- %v", elapsed, received, crawler.TotalMessages())
	if crawler.totalMessages != EXPECTED_URLS {
		t.Errorf("Expected %v urls but was %v", EXPECTED_URLS, crawler.totalMessages)
	}
	if err != nil {
		t.Errorf("Unexpected error %v", err)
	} else if EXPECTED_URLS != received {
		t.Errorf("Expected %v urls but was %v", EXPECTED_URLS, received)
	}
}

func TestCrawlWithTimeout(t *testing.T) {
	started := time.Now()
	timeout := time.Duration(4 * time.Millisecond)
	ctx, cancel := context.WithTimeout(context.Background(), timeout)
	defer cancel()
	crawler := New(ctx)
	received, err := crawler.Crawl(ctx, []string{"a.com", "b.com", "c.com", "d.com", "e.com", "f.com", "g.com", "h.com", "i.com", "j.com", "k.com", "l.com", "n.com"}, timeout)
	if err == nil {
		t.Errorf("Expecting timeout error")
	}
	elapsed := time.Since(started)
	log.Printf("Timedout took %s to process %v messages -- %v - %v", elapsed, received, crawler.TotalMessages(), err)
}

func TestCrawlWithCancel(t *testing.T) {
	started := time.Now()
	timeout := time.Duration(3 * time.Second)
	ctx, cancel := context.WithTimeout(context.Background(), timeout)
	crawler := New(ctx)
	var err error
	var received int
	go func() {
		// calling asynchronously
		received, err = crawler.Crawl(ctx, []string{"a.com", "b.com", "c.com", "d.com", "e.com", "f.com", "g.com", "h.com", "i.com", "j.com", "k.com", "l.com", "n.com"}, timeout)
	}()
	time.Sleep(5 * time.Millisecond)
	cancel()
	time.Sleep(50 * time.Millisecond)
	if err == nil {
		t.Errorf("Expecting cancel error")
	}
	elapsed := time.Since(started)
	log.Printf("Cancel took %s to process %v messages -- %v - %v", elapsed, received, crawler.TotalMessages(), err)
}
