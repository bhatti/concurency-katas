package crawler

import (
	"context"
	"log"
	"strings"
	"testing"
	"time"
)

const MAX_MESSAGES = 19032

func TestCrawl(t *testing.T) {
	started := time.Now()
	timeout := time.Duration(5 * time.Second)
	ctx, cancel := context.WithTimeout(context.Background(), timeout)
	defer cancel()
	crawler := New(ctx)
	crawler.Crawl(ctx, []string{"a.com", "b.com", "c.com", "d.com", "e.com", "f.com", "g.com", "h.com", "i.com", "j.com", "k.com", "l.com", "n.com"})
	received := 0
	for i := 0; i < MAX_MESSAGES; i++ {
		res := crawler.NextResult(timeout)
		if res.Error != nil {
			t.Errorf("Unexpected error %v", res.Error)
		}
		received++
	}
	elapsed := time.Since(started)
	log.Printf("Crawl took %s to process %v messages -- %v", elapsed, received, crawler.TotalMessages())
}

func TestCrawlWithTimeout(t *testing.T) {
	started := time.Now()
	timeout := time.Duration(5)
	ctx, cancel := context.WithTimeout(context.Background(), timeout)
	defer cancel()
	crawler := New(ctx)
	crawler.Crawl(ctx, []string{"a.com", "b.com", "c.com", "d.com", "e.com", "f.com", "g.com", "h.com", "i.com", "j.com", "k.com", "l.com", "n.com"})
	received := 0
	timedoutErrors := 0
	for i := 0; i < MAX_MESSAGES; i++ {
		res := crawler.NextResult(timeout)
		if res.Error != nil && strings.Contains(res.Error.Error(), "result_timedout") {
			timedoutErrors++
		}
		received++
	}
	if timedoutErrors == 0 {
		t.Errorf("Expecting timeout error")
	} else {
		log.Printf("Timedout %v requests", timedoutErrors)
	}
	elapsed := time.Since(started)
	log.Printf("Timedout took %s to process %v messages -- %v", elapsed, received, crawler.TotalMessages())
}

func TestCrawlWithCancel(t *testing.T) {
	started := time.Now()
	timeout := time.Duration(5 * time.Second)
	ctx, cancel := context.WithTimeout(context.Background(), timeout)
	crawler := New(ctx)
	crawler.Crawl(ctx, []string{"a.com", "b.com", "c.com", "d.com", "e.com", "f.com", "g.com", "h.com", "i.com", "j.com", "k.com", "l.com", "n.com"})
	received := 0
	// calling cancel explicitly
	for i := 0; i < 10; i++ {
		res := crawler.NextResult(timeout)
		if res.Error != nil {
			t.Errorf("Unexpected error %v", res.Error)
		}
		received++
	}
	cancel()
	res := crawler.NextResult(timeout)
	for res.Error == nil {
		res = crawler.NextResult(timeout)
	}
	if res.Error != nil && !strings.Contains(res.Error.Error(), "result_cancelled") {
		t.Errorf("Unexpected error %v", res.Error)
	}
	elapsed := time.Since(started)
	log.Printf("Cancel took %s to process %v messages -- %v", elapsed, received, crawler.TotalMessages())
}
