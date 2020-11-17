package domain

import (
	"context"
	"errors"
	"fmt"
	"time"
)

type State int

const (
	PENDING State = iota
	STARTED
	COMPLETED
	FAILED
)

type Handler interface {
	Handle(ctx context.Context, req Request) Result
}

// Request encapsulates details of url to crawl
type Request struct {
	URL       string
	Depth     int
	Timeout   time.Duration
	CreatedAt time.Time
}

func NewRequest(url string, depth int, timeout time.Duration) *Request {
	return &Request{URL: url, Depth: depth, Timeout: timeout, CreatedAt: time.Now()}
}

// Result encapsulates result of crawl
type Result struct {
	Status      State
	ChildURLs   int
	Error       error
	StartedAt   time.Time
	CompletedAt time.Time
}

func ResultTimedout(timeout time.Duration) Result {
	return Result{Status: FAILED, StartedAt: time.Now(), CompletedAt: time.Now(), Error: errors.New(fmt.Sprintf("result_timedout %v", timeout))}
}

func ResultCancelled() Result {
	return Result{Status: FAILED, StartedAt: time.Now(), CompletedAt: time.Now(), Error: errors.New("result_cancelled")}
}

func NewResult(req *Request) *Result {
	return &Result{Status: PENDING, StartedAt: time.Now()}
}

func (r *Result) Succeeded(total int) {
	r.ChildURLs = total
	r.Status = COMPLETED
	r.CompletedAt = time.Now()
}

func (r *Result) Failed(err error) {
	r.Status = FAILED
	r.Error = err
	r.CompletedAt = time.Now()
}
