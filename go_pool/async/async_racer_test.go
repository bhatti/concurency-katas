package async

import (
	"context"
	"fmt"
	"strings"
	"testing"
	"time"
)

func TestAsyncRacerWithTimeout(t *testing.T) {
	timeout := time.Duration(5 * time.Millisecond)
	ctx := context.Background()
	handler1 := func(ctx context.Context) (interface{}, error) {
		time.Sleep(100 * time.Millisecond)
		return 1, nil
	}
	handler2 := func(ctx context.Context) (interface{}, error) {
		time.Sleep(200 * time.Millisecond)
		return 2, nil
	}

	f, _ := ExecuteRacer(ctx, handler1, handler2)
	_, err := f.Await(ctx, timeout)
	if err == nil {
		t.Errorf("Expected error")
	} else if !strings.Contains(err.Error(), "deadline exceeded") {
		t.Errorf("Expected deadline exceeded but was %v", err)
	}
}

func TestAsyncRacerWithFirstWinner(t *testing.T) {
	timeout := time.Duration(10 * time.Millisecond)
	ctx := context.Background()
	handler1 := func(ctx context.Context) (interface{}, error) {
		time.Sleep(1 * time.Millisecond)
		return 1, nil
	}
	handler2 := func(ctx context.Context) (interface{}, error) {
		time.Sleep(20 * time.Millisecond)
		return 2, nil
	}

	f, _ := ExecuteRacer(ctx, handler1, handler2)
	r, err := f.Await(ctx, timeout)
	if err != nil {
		t.Errorf("Unexpected error %v", err)
	} else if r != 1 {
		t.Errorf("Unexpected result %v", r)
	}
}

func TestAsyncRacerWithSecondWinner(t *testing.T) {
	timeout := time.Duration(10 * time.Millisecond)
	ctx := context.Background()
	handler1 := func(ctx context.Context) (interface{}, error) {
		time.Sleep(10 * time.Millisecond)
		return 5, nil
	}
	handler2 := func(ctx context.Context) (interface{}, error) {
		time.Sleep(2 * time.Millisecond)
		return 2, nil
	}

	f, _ := ExecuteRacer(ctx, handler1, handler2)
	r, err := f.Await(ctx, timeout)
	if err != nil {
		t.Errorf("Unexpected error %v", err)
	} else if r != 2 {
		t.Errorf("Unexpected result %v", r)
	}
}

func TestAsyncRacerWithFirstFailure(t *testing.T) {
	timeout := time.Duration(10 * time.Millisecond)
	ctx := context.Background()
	handler1 := func(ctx context.Context) (interface{}, error) {
		time.Sleep(1 * time.Millisecond)
		return 1, fmt.Errorf("first failure")
	}
	handler2 := func(ctx context.Context) (interface{}, error) {
		time.Sleep(20 * time.Millisecond)
		return 2, nil
	}

	f, _ := ExecuteRacer(ctx, handler1, handler2)
	_, err := f.Await(ctx, timeout)
	if err == nil || !strings.Contains(err.Error(), "first failure") {
		t.Errorf("Unexpected error %v", err)
	}
}

func TestAsyncRacerWithSecondFailure(t *testing.T) {
	timeout := time.Duration(10 * time.Millisecond)
	ctx := context.Background()
	handler1 := func(ctx context.Context) (interface{}, error) {
		time.Sleep(10 * time.Millisecond)
		return 1, fmt.Errorf("first failure")
	}
	handler2 := func(ctx context.Context) (interface{}, error) {
		time.Sleep(2 * time.Millisecond)
		return 2, fmt.Errorf("second failure")
	}

	f, _ := ExecuteRacer(ctx, handler1, handler2)
	_, err := f.Await(ctx, timeout)
	if err == nil || !strings.Contains(err.Error(), "second failure") {
		t.Errorf("Unexpected error %v", err)
	}
}
