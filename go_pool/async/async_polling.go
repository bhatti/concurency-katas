package async

import (
	"context"
	"errors"
	"fmt"
	"time"
)

// type of function that is used to top repeated function
type pollingCompletionHandler func(
	ctx context.Context,
	payload interface{}) (bool, interface{}, error)

// PollingAwaiter defines method to wait for result
type PollingAwaiter interface {
	Await(ctx context.Context, timeout time.Duration) (bool, interface{}, error)
	IsRunning() bool
}

// pollingTask - processes data asynchronously
type pollingTask struct {
	completionHandler pollingCompletionHandler
	abortHandler      AbortHandler
	request           interface{}
	resultQ           chan pollingResponse
	pollInterval      time.Duration
	running           bool
}

// pollingResponse encapsulates results of async repeated task
type pollingResponse struct {
	response
	completed bool
}

// ExecutePolling executes a function repeatedly polls a background task until it is completed
func ExecutePolling(
	ctx context.Context,
	completionHandler pollingCompletionHandler,
	abortHandler AbortHandler,
	request interface{},
	pollInterval time.Duration) PollingAwaiter {
	task := &pollingTask{
		request:           request,
		abortHandler:      abortHandler,
		completionHandler: completionHandler,
		resultQ:           make(chan pollingResponse, 1),
		pollInterval:      pollInterval,
		running:           true,
	}
	go task.run(ctx) // run handler asynchronously
	return task
}

// IsRunning checks if task is still running
func (t *pollingTask) IsRunning() bool {
	return t.running
}

// Await waits for completion of the task
func (t *pollingTask) Await(
	ctx context.Context,
	timeout time.Duration) (completed bool, result interface{}, err error) {
	completed = false
	result = nil
	select {
	case <-ctx.Done():
		if ctx.Err() != nil {
			err = ctx.Err()
		} else {
			err = errors.New("await canceled")
		}
	case res := <-t.resultQ:
		result = res.result
		err = res.err
		completed = res.completed
	case <-time.After(timeout):
		err = errors.New(fmt.Sprintf("async task timedout %v", timeout))
	}
	t.running = false
	if err != nil {
		go t.abortHandler(ctx, t.request) // abortHandler operation
	}
	return
}

////////////////////////////////////// PRIVATE METHODS ///////////////////////////////////////
func (t *pollingTask) invokeHandlerAndCheckCompletion(ctx context.Context) bool {
	completed, result, err := t.completionHandler(ctx, t.request)
	if completed || err != nil {
		t.resultQ <- pollingResponse{
			response:  response{result: result, err: err},
			completed: completed,
		}
		close(t.resultQ) // notify wait task
		return completed
	}
	return false
}

func (t *pollingTask) run(ctx context.Context) {
	go func() {
		for {
			if t.invokeHandlerAndCheckCompletion(ctx) {
				break
			}
			select {
			case <-ctx.Done():
				break
			case <-time.After(t.pollInterval):
				continue
			}
		}
	}()
}
