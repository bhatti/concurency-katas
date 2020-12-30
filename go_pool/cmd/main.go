package main

import (
	"context"
	"log"
	"time"

	"plexobject.com/crawler"
)

func main() {
	started := time.Now()
	timeout := time.Duration(6 * time.Second)
	ctx, cancel := context.WithTimeout(context.Background(), timeout)
	defer cancel()
	c := crawler.New(ctx)
	received, err := c.Crawl(ctx, []string{"a.com", "b.com", "c.com", "d.com", "e.com", "f.com", "g.com", "h.com", "i.com", "j.com", "k.com", "l.com", "n.com"}, timeout)
	if err != nil {
		panic(err)
	}
	elapsed := time.Since(started)
	log.Printf("Crawl took %s to process %v messages -- %v", elapsed, received, c.TotalMessages())
}
