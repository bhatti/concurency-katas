package async

import (
	"context" //"strings"
	"errors"
	"log"
	"strings"
	"testing"
	"time"
)

func fib(n uint) uint64 {
	if n <= 1 {
		return uint64(n)
	}
	return fib(n-1) + fib(n-2)
}

func TestAsyncWithSleep(t *testing.T) {
	started := time.Now()
	ctx, cancel := context.WithTimeout(context.Background(), time.Duration(10*time.Second))
	defer cancel()
	handler := func(ctx context.Context, payload interface{}) (interface{}, error) {
		return fib(10000), nil
	}
	taskQueue := New(handler)
	future := taskQueue.Async(ctx, 10)
	_, err := future.Await(ctx, time.Duration(10*time.Millisecond))
	elapsed := time.Since(started)
	log.Printf("TestAsyncWithSleep took %s", elapsed)
	if err == nil {
		t.Errorf("Expected error")
	} else if !strings.Contains(err.Error(), "async_timedout") {
		t.Errorf("Expected timeout error but was %v", err)
	}
}

func TestAsyncWithTimeout(t *testing.T) {
	started := time.Now()
	timeout := time.Duration(10 * time.Millisecond)
	ctx, cancel := context.WithTimeout(context.Background(), timeout)
	defer cancel()
	handler := func(ctx context.Context, payload interface{}) (interface{}, error) {
		val := payload.(int)
		if val%2 == 0 {
			time.Sleep(100 * time.Millisecond)
		}
		return val * val, nil
	}
	taskQueue := New(handler)
	futures := make([]AsyncAwaiter, 0)
	for i := 1; i <= 4; i++ {
		futures = append(futures, taskQueue.Async(ctx, i))
	}
	sum := 0
	var savedError error
	for i := 0; i < len(futures); i++ {
		res, err := futures[i].Await(ctx, timeout)
		if res != nil {
			sum += res.(int)
		} else if err != nil {
			savedError = err
		}
	}
	elapsed := time.Since(started)
	log.Printf("TestAsyncWithTimeout took %s", elapsed)
	if savedError == nil {
		t.Errorf("Expected error")
	} else if !strings.Contains(savedError.Error(), "async_cancelled") {
		t.Errorf("Expected fake even error but was %v", savedError)
	}
}

func TestAsyncWithFailure(t *testing.T) {
	started := time.Now()
	timeout := time.Duration(2 * time.Second)
	ctx, cancel := context.WithTimeout(context.Background(), timeout)
	defer cancel()
	handler := func(ctx context.Context, payload interface{}) (interface{}, error) {
		val := payload.(int)
		if val%2 == 0 {
			return nil, errors.New("fake even error")
		}
		return val * val, nil
	}
	taskQueue := New(handler)
	futures := make([]AsyncAwaiter, 0)
	for i := 1; i <= 4; i++ {
		futures = append(futures, taskQueue.Async(ctx, i))
	}
	sum := 0
	var savedError error
	for i := 0; i < len(futures); i++ {
		res, err := futures[i].Await(ctx, timeout)
		if res != nil {
			sum += res.(int)
		} else if err != nil {
			savedError = err
		}
	}
	elapsed := time.Since(started)
	log.Printf("TestAsyncWithFailure took %s", elapsed)
	expected := 1 + 9
	if sum != expected {
		t.Errorf("Expected %v but was %v", expected, sum)
	}
	if savedError == nil {
		t.Errorf("Expected error")
	} else if !strings.Contains(savedError.Error(), "fake even") {
		t.Errorf("Expected fake even error but was %v", savedError)
	}
}

func TestAsync(t *testing.T) {
	started := time.Now()
	timeout := time.Duration(2 * time.Second)
	ctx, cancel := context.WithTimeout(context.Background(), timeout)
	defer cancel()
	handler := func(ctx context.Context, payload interface{}) (interface{}, error) {
		val := payload.(int)
		return val * val, nil
	}
	taskQueue := New(handler)
	futures := make([]AsyncAwaiter, 0)
	for i := 1; i <= 4; i++ {
		futures = append(futures, taskQueue.Async(ctx, i))
	}
	sum := 0
	for i := 0; i < len(futures); i++ {
		res, _ := futures[i].Await(ctx, timeout)
		if res != nil {
			sum += res.(int)
		}
	}
	elapsed := time.Since(started)
	log.Printf("TestAsync took %s", elapsed)
	expected := 1 + 4 + 9 + 16
	if sum != expected {
		t.Errorf("Expected %v but was %v", expected, sum)
	}
}
