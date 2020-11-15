package queue

import (
	"context"
	"time"

	"plexobject.com/crawler/domain"
)

const BUFFER_CAPACITY = 10 // allow buffering to support asynchronous behavior  as by default sender will be blocked

// TaskQueue - serves tasks in asynchronous queue using goroutines
type TaskQueue struct {
	size     int
	jobsQ    chan domain.Request
	resultsQ chan domain.Result
	handler  domain.Handler
}

func New(ctx context.Context, size int, handler domain.Handler) *TaskQueue {
	jobsQ := make(chan domain.Request, BUFFER_CAPACITY)
	resultsQ := make(chan domain.Result, BUFFER_CAPACITY)
	queue := &TaskQueue{size: size, jobsQ: jobsQ, resultsQ: resultsQ, handler: handler}
	for w := 1; w <= size; w++ {
		go queue.worker(ctx)
	}
	return queue
}

func (w *TaskQueue) Add(req domain.Request) {
	// Sending messages asynchronously in goroutine otherwise it will block sender
	go func() {
		w.jobsQ <- req
	}()
}

func (w *TaskQueue) NextResult(timeout time.Duration) domain.Result {
	select {
	case res := <-w.resultsQ:
		return res
	case <-time.After(timeout):
		return domain.ResultTimedout(timeout)
	}
}

func (w *TaskQueue) Stop() {
	close(w.jobsQ)
	close(w.resultsQ)
}

func (w *TaskQueue) worker(ctx context.Context) {
	for {
		select {
		case <-ctx.Done():
			w.resultsQ <- domain.ResultCancelled()
			return
		case req := <-w.jobsQ:
			res := w.handler.Handle(ctx, req)
			w.resultsQ <- res
		}
	}
}
