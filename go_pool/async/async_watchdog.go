package async

import (
	"context"
	"time"
)

// type of function that is used to check for status on environment and returns error if task needs
// to be aborted
type watchdogHandler func(ctx context.Context, payload interface{}) error

// watchdogTask - processes data asynchronously
type watchdogTask struct {
	handler      Handler
	errorHandler watchdogHandler
	abortHandler AbortHandler
	request      interface{}
	resultQ      chan Response
	errorQ       chan error
	pollInterval time.Duration
	running      bool
}

// ExecuteWatchdog executes a long-running handler asynchronously and also invokes a watchdog
// function repeatedly until either the main handler or watch fails, e.g. you can
// submit a long-running asynchronous task and use watchdog handler to poll the environment
// and check for errors. If watchdog fails, it will return early and optionally cancel long-running
// task if abortHandler is provided.
func ExecuteWatchdog(
	ctx context.Context,
	handler Handler,
	errorHandler watchdogHandler,
	abortHandler AbortHandler,
	request interface{},
	pollInterval time.Duration) Awaiter {
	task := &watchdogTask{
		request:      request,
		handler:      handler,
		abortHandler: abortHandler,
		errorHandler: errorHandler,
		resultQ:      make(chan Response, 1),
		errorQ:       make(chan error, 1),
		pollInterval: pollInterval,
		running:      true,
	}
	go task.runMain(ctx)     // run main handler asynchronously
	go task.runWatchdog(ctx) // run watchdog handler asynchronously
	return task
}

// IsRunning checks if task is still running
func (t *watchdogTask) IsRunning() bool {
	return t.running
}

// Await waits for completion of the task
func (t *watchdogTask) Await(
	ctx context.Context,
	timeout time.Duration) (result interface{}, err error) {
	ctx, cancel := context.WithTimeout(ctx, timeout)
	defer cancel()
	result = nil
	select {
	case <-ctx.Done():
		err = ctx.Err()
	case res := <-t.resultQ:
		result = res.Result
		err = res.Err
	case err = <-t.errorQ:
	}
	if err != nil {
		go t.abortHandler(ctx, t.request) // abortHandler operation
	}
	return
}

////////////////////////////////////// PRIVATE METHODS ///////////////////////////////////////
func (t *watchdogTask) invokeErrorHandler(ctx context.Context) error {
	err := t.errorHandler(ctx, t.request)
	if err != nil {
		t.errorQ <- err
		t.running = false
		close(t.errorQ)  // notify wait task
		close(t.resultQ) // end go-routine for main handler
		return err
	}
	return nil
}

func (t *watchdogTask) runMain(ctx context.Context) {
	go func() {
		result, err := t.handler(ctx, t.request)
		t.resultQ <- Response{Result: result, Err: err} // out channel is buffered by 1
		t.running = false
		close(t.resultQ) // notify wait task
		close(t.errorQ)  // end go-routine for watchdog
	}()
}

func (t *watchdogTask) runWatchdog(ctx context.Context) {
	go func() {
		for {
			if t.invokeErrorHandler(ctx) != nil {
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
