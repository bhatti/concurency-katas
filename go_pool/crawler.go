package crawler

import (
	"context"
	"errors"
	"fmt"
	"math/rand"
	"sync/atomic"
	"time"

	"plexobject.com/crawler/domain"
	"plexobject.com/crawler/queue"
)

const MAX_DEPTH = 4
const MAX_URLS = 11
const MAX_WORKERS = 1000

// Crawler is used for crawing URLs
type Crawler struct {
	taskQueue     *queue.TaskQueue
	totalMessages uint64
}

// Instantiates new crawler
func New(ctx context.Context) *Crawler {
	crawler := &Crawler{totalMessages: 0}
	crawler.taskQueue = queue.New(ctx, MAX_WORKERS, crawler)
	return crawler
}

// Stops all workers in task queue
func (c *Crawler) Stop() {
	c.taskQueue.Stop()
}

// Crawls list of URLs with specified depth
func (c *Crawler) Crawl(ctx context.Context, urls []string) error {
	return c.crawl(ctx, urls, 0)
}

func (c *Crawler) TotalMessages() uint64 {
	return c.totalMessages
}

func (c *Crawler) NextResult(timeout time.Duration) domain.Result {
	return c.taskQueue.NextResult(timeout)
}

// Implementing Handler interface
func (c *Crawler) Handle(ctx context.Context, req domain.Request) domain.Result {
	atomic.AddUint64(&c.totalMessages, 1)
	res := domain.NewResult(req)
	if err, contents := download(ctx, req.URL); err != nil {
		res.Failed(err)
		return res
	} else {
		if err, newContents := jsrender(ctx, req.URL, contents); err != nil {
			res.Failed(err)
			return res
		} else {
			if hasContentsChanged(ctx, req.URL, newContents) && !isSpam(ctx, req.URL, newContents) {
				index(ctx, req.URL, newContents)
				urls := parseURLs(ctx, req.URL, newContents)
				c.crawl(ctx, urls, req.Depth+1)
				res.Succeeded()
			} else {
				res.Failed(errors.New("contents didn't change"))
			}
		}
	}

	return res
}

/////////////////// Internal private methods ///////////////////////////
// Crawls list of URLs with specified depth
func (c *Crawler) crawl(ctx context.Context, urls []string, depth int) error {
	if depth < MAX_DEPTH {
		for i := 0; i < len(urls); i++ {
			c.taskQueue.Add(domain.NewRequest(urls[i], depth))
		}
		return nil
	} else {
		return errors.New("depth exceeded")
	}
}

func download(ctx context.Context, url string) (error, string) {
	// TODO check robots.txt and throttle policies
	// TODO add timeout for slow websites and linearize requests to the same domain to prevent denial of service attack
	return nil, randomString(100)
}

func jsrender(ctx context.Context, url string, contents string) (error, string) {
	// for SPA apps that use javascript for rendering contents
	return nil, contents
}

func index(ctx context.Context, url string, contents string) error {
	// apply standardize, stem, ngram, etc for indexing
	return nil
}

func parseURLs(ctx context.Context, url string, contents string) []string {
	// tokenize contents and extract href/image/script urls
	urls := make([]string, 0)
	for i := 0; i < MAX_URLS; i++ {
		urls = append(urls, randomUrl())
	}
	return urls
}

func hasContentsChanged(ctx context.Context, url string, contents string) bool {
	return true
}

func isSpam(ctx context.Context, url string, contents string) bool {
	return false
}

func randomString(n int) string {
	var letters = []rune("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")

	s := make([]rune, n)
	for i := range s {
		s[i] = letters[rand.Intn(len(letters))]
	}
	return string(s)
}

func randomUrl() string {
	domains := []string{"ab.com", "bc.com", "cd.com", "de.com", "ef.com", "fg.com", "gh.com", "hi.com", "ij.com", "jk.com", "kl.com", "lm.com", "mn.com",
		"no.com", "op.com", "pq.com", "qr.com", "rs.com", "st.com", "tu.com", "uv.com", "vw.com", "wx.com", "xy.com", "yz.com"}
	i := rand.Intn(len(domains))
	domain := domains[i]
	return fmt.Sprintf("https://%v/%v", domain, randomString(20))
}
