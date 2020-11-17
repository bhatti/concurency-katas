package pool

import (
	"context"
	"errors"
	"fmt"
	"time"

	"github.com/google/uuid"
)

const BUFFER_CAPACITY = 2 // allow buffering to support asynchronous behavior  as by default sender will be blocked

type Handler func(ctx context.Context, payload interface{}) (interface{}, error)

type Awaiter interface {
	Await(ctx context.Context, timeout time.Duration) (interface{}, error)
}

// Request encapsulates request to process
type Request struct {
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

type Worker struct {
	id                   int
	handler              Handler
	workerRequestChannel chan *Request
}

func NewWorker(id int, handler Handler) Worker {
	return Worker{
		id:                   id,
		handler:              handler,
		workerRequestChannel: make(chan *Request),
	}
}

func (w Worker) start(ctx context.Context, workersReadyPool chan chan *Request, done chan bool) {
	go func(w Worker) {
		for {
			// register the current worker into the worker queue.
			workersReadyPool <- w.workerRequestChannel

			select {
			case <-ctx.Done():
				break
			case req := <-w.workerRequestChannel:
				payload, err := w.handler(ctx, req.payload)
				req.outQ <- Result{id: req.id, payload: payload, err: err} // out channel is buffered by 1
			//close(req.outQ) // optionally close as we only need a single reply
			case <-done:
				return
			}
		}
	}(w)
}

// WorkPool - pool of workers
type WorkPool struct {
	size                int
	workersReadyPool    chan chan *Request
	pendingRequestQueue chan *Request
	done                chan bool
	handler             Handler
}

func New(handler Handler, size int) *WorkPool {
	async := &WorkPool{
		size:                size,
		workersReadyPool:    make(chan chan *Request, BUFFER_CAPACITY),
		pendingRequestQueue: make(chan *Request, BUFFER_CAPACITY),
		done:                make(chan bool),
		handler:             handler}
	return async
}

func (p *WorkPool) Start(ctx context.Context) {
	for w := 1; w <= p.size; w++ {
		worker := NewWorker(w, p.handler)
		worker.start(ctx, p.workersReadyPool, p.done)
	}
	go p.dispatch(ctx)
}

func (p *WorkPool) Add(ctx context.Context, payload interface{}) Awaiter {
	// Adding request to process
	req := &Request{id: uuid.New().String(), payload: payload, outQ: make(chan Result, 1)}
	go func() {
		p.pendingRequestQueue <- req
	}()
	return req
}

func (r Request) Await(ctx context.Context, timeout time.Duration) (payload interface{}, err error) {
	select {
	case <-ctx.Done():
		err = errors.New("async_cancelled")
	case res := <-r.outQ:
		payload = res.payload
		err = res.err
	case <-time.After(timeout):
		payload = nil
		err = errors.New(fmt.Sprintf("async_timedout %v", timeout))
	}

	return
}

func (p *WorkPool) Stop() {
	close(p.pendingRequestQueue)
	go func() {
		p.done <- true
	}()
}

// Receiving requests from inbound jobQuee and forward it to the worker's workerRequestChannel
func (p *WorkPool) dispatch(ctx context.Context) {
	for {
		select {
		case <-ctx.Done():
			return
		case <-p.done:
			return
		case req := <-p.pendingRequestQueue:
			go func(req *Request) {
				// Find next worker
				workerRequestChannel := <-p.workersReadyPool
				// dispatch the request to next worker
				workerRequestChannel <- req
			}(req)
		}
	}
}
