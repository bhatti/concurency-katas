package async

import (
	"context"
	"errors"
	"fmt"
	"time"

	"github.com/google/uuid"
)

type AsyncHandler func(ctx context.Context, payload interface{}) (interface{}, error)

type AsyncAwaiter interface {
	Await(ctx context.Context, timeout time.Duration) (interface{}, error)
}

// Future encapsulates request to process asynchronously
type Future struct {
	id      string
	payload interface{}
	outQ    chan Result
}

// Result encapsulates results
type Result struct {
	id      string
	payload interface{}
	err     error
}

// Async - processes data asynchronously
type Async struct {
	handler AsyncHandler
}

func New(handler AsyncHandler) *Async {
	async := &Async{handler: handler}
	return async
}

func (a *Async) Async(ctx context.Context, payload interface{}) AsyncAwaiter {
	future := &Future{id: uuid.New().String(), payload: payload, outQ: make(chan Result, 1)}
	go future.run(ctx, a.handler) // run handler asynchronously
	return future
}

func (f Future) run(ctx context.Context, handler AsyncHandler) {
	go func() {
		payload, err := handler(ctx, f.payload)
		f.outQ <- Result{id: f.id, payload: payload, err: err} // out channel is buffered by 1
		close(f.outQ)
	}()
}

func (f Future) Await(ctx context.Context, timeout time.Duration) (payload interface{}, err error) {
	payload = nil
	select {
	case <-ctx.Done():
		err = errors.New("async_cancelled")
	case res := <-f.outQ:
		payload = res.payload
		err = res.err
	case <-time.After(timeout):
		err = errors.New(fmt.Sprintf("async_timedout %v", timeout))
	}

	return
}
