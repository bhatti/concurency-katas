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

// Promise encapsulates request to process asynchronously
type Promise struct {
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
	p := &Promise{id: uuid.New().String(), payload: payload, outQ: make(chan Result, 1)}
	go p.run(ctx, a.handler) // run handler asynchronously
	return p
}

func (p Promise) run(ctx context.Context, handler AsyncHandler) {
	go func() {
		payload, err := handler(ctx, p.payload)
		p.outQ <- Result{id: p.id, payload: payload, err: err} // out channel is buffered by 1
		close(p.outQ)
	}()
}

func (p Promise) Await(ctx context.Context, timeout time.Duration) (payload interface{}, err error) {
	payload = nil
	select {
	case <-ctx.Done():
		err = errors.New("async_cancelled")
	case res := <-p.outQ:
		payload = res.payload
		err = res.err
	case <-time.After(timeout):
		err = errors.New(fmt.Sprintf("async_timedout %v", timeout))
	}

	return
}
