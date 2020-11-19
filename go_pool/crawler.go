//--------------------------------------------------------------------------
// fake web crawler using async API built on top of goroutines and channels
//--------------------------------------------------------------------------

package crawler

import (
	"context"
	"errors"
	"sync/atomic"
	"time"

	"plexobject.com/crawler/async"
	"plexobject.com/crawler/domain"
	"plexobject.com/crawler/utils"
)

const MAX_DEPTH = 4
const MAX_URLS = 11

// Crawler is used for crawing URLs
type Crawler struct {
	taskQueue     *async.Async
	downloader    *async.Async
	jsrenderer    *async.Async
	indexer       *async.Async
	totalMessages uint64
}

// Instantiates new crawler
func New(ctx context.Context) *Crawler {
	crawler := &Crawler{totalMessages: 0}
	crawlHandler := func(ctx context.Context, payload interface{}) (interface{}, error) {
		req := payload.(*domain.Request)
		return crawler.handleCrawl(ctx, req)
	}
	downloader := func(ctx context.Context, payload interface{}) (interface{}, error) {
		// TODO check robots.txt and throttle policies
		// TODO add timeout for slow websites and linearize requests to the same domain to prevent denial of service attack
		return utils.RandomString(100), nil
	}
	renderer := func(ctx context.Context, payload interface{}) (interface{}, error) {
		// for SPA apps that use javascript for rendering contents
		return utils.RandomString(100), nil
	}
	indexer := func(ctx context.Context, payload interface{}) (interface{}, error) {
		return 0, nil
	}
	crawler.taskQueue = async.New(crawlHandler)
	crawler.downloader = async.New(downloader)
	crawler.jsrenderer = async.New(renderer)
	crawler.indexer = async.New(indexer)
	return crawler
}

// Crawls list of URLs with specified depth
func (c *Crawler) Crawl(ctx context.Context, urls []string, timeout time.Duration) (int, error) {
	// Boundary for concurrency and it will not return until all
	// child URLs are crawled up to MAX_DEPTH limit.
	return c.crawl(ctx, urls, 0, timeout)
}

func (c *Crawler) TotalMessages() uint64 {
	return c.totalMessages
}

// handles crawl
func (c *Crawler) handleCrawl(ctx context.Context, req *domain.Request) (*domain.Result, error) {
	atomic.AddUint64(&c.totalMessages, 1)
	timeout := time.Duration(req.Timeout * time.Second)
	res := domain.NewResult(req)
	if contents, err := c.downloader.Async(ctx, req.URL).Await(ctx, timeout); err != nil {
		res.Failed(err)
	} else {
		if newContents, err := c.jsrenderer.Async(ctx, [...]string{req.URL, contents.(string)}).Await(ctx, timeout); err != nil {
			res.Failed(err)
		} else {
			if hasContentsChanged(ctx, req.URL, newContents.(string)) && !isSpam(ctx, req.URL, newContents.(string)) {
				c.indexer.Async(ctx, [...]string{req.URL, newContents.(string)}).Await(ctx, timeout)
				urls := parseURLs(ctx, req.URL, newContents.(string))
				if childURLs, err := c.crawl(ctx, urls, req.Depth+1, req.Timeout); err != nil {
					res.Failed(err)
				} else {
					res.Succeeded(childURLs + 1)
				}
			} else {
				res.Failed(errors.New("contents didn't change"))
			}
		}
	}

	return res, nil
}

/////////////////// Internal private methods ///////////////////////////
// Crawls list of URLs with specified depth
func (c *Crawler) crawl(ctx context.Context, urls []string, depth int, timeout time.Duration) (int, error) {
	if depth < MAX_DEPTH {
		futures := make([]async.AsyncAwaiter, 0)
		for i := 0; i < len(urls); i++ {
			futures = append(futures, c.taskQueue.Async(ctx, domain.NewRequest(urls[i], depth, timeout)))
		}
		sum := 0
		var savedError error
		for i := 0; i < len(futures); i++ {
			res, err := futures[i].Await(ctx, timeout)
			if err != nil {
				savedError = err // returning only a single error
			}
			if res != nil {
				sum += res.(*domain.Result).ChildURLs
			}
		}

		return sum, savedError
	} else {
		return 0, nil
	}
}

func parseURLs(ctx context.Context, url string, contents string) []string {
	// tokenize contents and extract href/image/script urls
	urls := make([]string, 0)
	for i := 0; i < MAX_URLS; i++ {
		urls = append(urls, utils.RandomChildUrl(url))
	}
	return urls
}

func hasContentsChanged(ctx context.Context, url string, contents string) bool {
	return true
}

func isSpam(ctx context.Context, url string, contents string) bool {
	return false
}
