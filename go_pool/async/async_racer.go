package async

import (
	"context"
	"fmt"
	"time"
)

const maxRacingHandlers = 3

// NoArgHandler  type of async function without args
type NoArgHandler func(ctx context.Context) (interface{}, error)

// racerTask - spawns multiple processes data asynchronously and returns first one that completes
type racerTask struct {
	handlers []NoArgHandler
	resultQ  []chan Response
	running  bool
}

// ExecuteRacer executes multiple processes asynchronously and returns first one that finishes
func ExecuteRacer(
	ctx context.Context,
	handlers ...NoArgHandler) (Awaiter, error) {
	if len(handlers) > 3 || len(handlers) == 0 {
		return nil, fmt.Errorf("Unsupported number of handlers")
	}
	resultQ := make([]chan Response, maxRacingHandlers)
	for i := 0; i < maxRacingHandlers; i++ {
		resultQ[i] = make(chan Response, 1)
	}
	task := &racerTask{
		handlers: handlers,
		resultQ:  resultQ,
		running:  true,
	}
	for i := 0; i < len(handlers); i++ {
		go task.run(ctx, handlers[i], resultQ[i])
	}
	return task, nil
}

// IsRunning checks if task is still running
func (t *racerTask) IsRunning() bool {
	return t.running
}

// Await waits for completion of the task
func (t *racerTask) Await(
	ctx context.Context,
	timeout time.Duration) (result interface{}, err error) {
	ctx, cancel := context.WithTimeout(ctx, timeout)
	defer cancel()
	result = nil
	select {
	case <-ctx.Done():
		err = ctx.Err()
	case res := <-t.resultQ[0]:
		result = res.Result
		err = res.Err
	case res := <-t.resultQ[1]:
		result = res.Result
		err = res.Err
	case res := <-t.resultQ[2]:
		result = res.Result
		err = res.Err
	}
	return
}

////////////////////////////////////// PRIVATE METHODS ///////////////////////////////////////
func (t *racerTask) run(ctx context.Context, handler NoArgHandler, resultQ chan Response) {
	go func() {
		result, err := handler(ctx)
		resultQ <- Response{Result: result, Err: err} // out channel is buffered by 1
		t.running = false
		close(resultQ) // notify wait task
	}()
}
