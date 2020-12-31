package async

import (
	"context"
	"errors"
	"fmt"
	"time"
)

// type of async function
type Handler func(ctx context.Context, request interface{}) (interface{}, error)

// type of abortHandler function that is called if async operation is cancelled
type AbortHandler func(ctx context.Context, request interface{}) (interface{}, error)

func NoAbort(ctx context.Context, request interface{}) (interface{}, error) {
	return nil, nil
}

// Awaiter - defines method to wait for result
type Awaiter interface {
	Await(ctx context.Context, timeout time.Duration) (interface{}, error)
	IsRunning() bool
}

// task - submits task asynchronously
type task struct {
	handler      Handler
	abortHandler AbortHandler
	request      interface{}
	resultQ      chan response
	running      bool
}

// response encapsulates results of async task
type response struct {
	result interface{}
	err    error
}

// Execute executes a long-running function in background and returns a future to wait for the response
func Execute(
	ctx context.Context,
	handler Handler,
	abortHandler AbortHandler,
	request interface{}) Awaiter {
	task := &task{
		request:      request,
		handler:      handler,
		abortHandler: abortHandler,
		resultQ:      make(chan response, 1),
		running:      true,
	}
	go task.run(ctx) // run handler asynchronously
	return task
}

// IsRunning checks if task is still running
func (t *task) IsRunning() bool {
	return t.running
}

// Await waits for completion of the task
func (t *task) Await(
	ctx context.Context,
	timeout time.Duration) (result interface{}, err error) {
	result = nil
	select {
	case <-ctx.Done():
		if ctx.Err() != nil {
			err = ctx.Err()
		} else {
			err = errors.New("async task cancelled")
		}
	case res := <-t.resultQ:
		result = res.result
		err = res.err
	case <-time.After(timeout):
		err = fmt.Errorf("async task timedout %v", timeout)
	}
	if err != nil {
		go t.abortHandler(ctx, t.request) // abortHandler operation
	}
	return
}

////////////////////////////////////// PRIVATE METHODS ///////////////////////////////////////
func (t *task) run(ctx context.Context) {
	go func() {
		result, err := t.handler(ctx, t.request)
		t.resultQ <- response{result: result, err: err} // out channel is buffered by 1
		t.running = false
		close(t.resultQ)
	}()
}
