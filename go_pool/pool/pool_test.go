package pool

import (
	"context" //"strings"
	"errors"
	"log"
	"strings"
	"testing"
	"time"
)

func TestWithTimeout(t *testing.T) {
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
	taskQueue := New(handler, 20)
	taskQueue.Start(ctx)
	futures := make([]Awaiter, 0)
	for i := 1; i <= 4; i++ {
		futures = append(futures, taskQueue.Add(ctx, i))
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
	log.Printf("TestWithTimeout took %s", elapsed)
	if savedError == nil {
		t.Errorf("Expected error")
	} else if !strings.Contains(savedError.Error(), "cancelled") {
		t.Errorf("Expected fake even error but was %v", savedError)
	}
}

func TestWithFailure(t *testing.T) {
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
	taskQueue := New(handler, 20)
	taskQueue.Start(ctx)
	futures := make([]Awaiter, 0)
	for i := 1; i <= 4; i++ {
		futures = append(futures, taskQueue.Add(ctx, i))
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
	log.Printf("TestWithFailure took %s", elapsed)
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

func TestAddAwait(t *testing.T) {
	started := time.Now()
	timeout := time.Duration(2 * time.Second)
	ctx, cancel := context.WithTimeout(context.Background(), timeout)
	defer cancel()
	handler := func(ctx context.Context, payload interface{}) (interface{}, error) {
		val := payload.(int)
		return val * val, nil
	}
	taskQueue := New(handler, 20)
	taskQueue.Start(ctx)
	futures := make([]Awaiter, 0)
	for i := 1; i <= 4; i++ {
		futures = append(futures, taskQueue.Add(ctx, i))
	}
	sum := 0
	for i := 0; i < len(futures); i++ {
		res, _ := futures[i].Await(ctx, timeout)
		if res != nil {
			sum += res.(int)
		}
	}
	elapsed := time.Since(started)
	log.Printf("TestAddAwait took %s", elapsed)
	expected := 1 + 4 + 9 + 16
	if sum != expected {
		t.Errorf("Expected %v but was %v", expected, sum)
	}
}
