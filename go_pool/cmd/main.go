package main

import (
	"context"
	"log"
	"time"

	"plexobject.com/crawler"
)

const MAX_MESSAGES = 14443

func main() {
	started := time.Now()
	timeout := time.Duration(6 * time.Second)
	ctx, cancel := context.WithTimeout(context.Background(), timeout)
	defer cancel()
	c := crawler.New(ctx)
	c.Crawl(ctx, []string{"a.com", "b.com", "c.com", "d.com", "e.com", "f.com", "g.com", "h.com", "i.com", "j.com", "k.com", "l.com", "n.com"})
	received := 0
	elapsed := time.Since(started)
	log.Printf("Sent in %v, waiting for %v messages", elapsed, MAX_MESSAGES)
	for i := 0; i < MAX_MESSAGES; i++ {
		c.NextResult(timeout)
		received++
	}
	elapsed = time.Since(started)
	log.Printf("Crawl took %s to process %v messages -- %v", elapsed, received, c.TotalMessages())
}
