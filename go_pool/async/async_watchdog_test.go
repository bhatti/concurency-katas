package async

import (
	"context"
	"fmt"
	"strings"
	"testing"
	"time"
)

func TestAsyncWatchdogWithTimeout(t *testing.T) {
	poll := time.Duration(1 * time.Millisecond)
	timeout := time.Duration(5 * time.Millisecond)
	ctx, cancel := context.WithTimeout(context.Background(), timeout)
	defer cancel()
	handler := func(ctx context.Context, payload interface{}) (interface{}, error) {
		time.Sleep(100 * time.Millisecond)
		return nil, nil
	}

	errorHandler := func(ctx context.Context, payload interface{}) error {
		return nil
	}

	future := ExecuteWatchdog(ctx, handler, errorHandler, NoAbort, 0, poll)
	_, err := future.Await(ctx, timeout)
	if err == nil {
		t.Errorf("Expected error")
	} else if !strings.Contains(err.Error(), "timedout") {
		t.Errorf("Expected timedout error but was %v", err)
	}
}

func TestAsyncWatchdogWithMainFailure(t *testing.T) {
	poll := time.Duration(1 * time.Millisecond)
	timeout := time.Duration(10 * time.Millisecond)
	ctx, cancel := context.WithTimeout(context.Background(), timeout)
	defer cancel()
	handler := func(ctx context.Context, payload interface{}) (interface{}, error) {
		time.Sleep(5 * time.Millisecond)
		return 0, fmt.Errorf("fake main error")
	}
	errorHandler := func(ctx context.Context, payload interface{}) error {
		return nil
	}

	future := ExecuteWatchdog(ctx, handler, errorHandler, NoAbort, 0, poll)
	_, err := future.Await(ctx, timeout)
	if err == nil {
		t.Errorf("Expected error")
	} else if !strings.Contains(err.Error(), "fake main") {
		t.Errorf("Expected fake main error but was %v", err)
	}
}

func TestAsyncWatchdogWithWatchdogFailure(t *testing.T) {
	poll := time.Duration(1 * time.Millisecond)
	timeout := time.Duration(10 * time.Millisecond)
	ctx, cancel := context.WithTimeout(context.Background(), timeout)
	defer cancel()
	handler := func(ctx context.Context, payload interface{}) (interface{}, error) {
		time.Sleep(25 * time.Millisecond)
		return 20, nil
	}
	errorHandler := func(ctx context.Context, payload interface{}) error {
		return fmt.Errorf("watchdog error")
	}

	future := ExecuteWatchdog(ctx, handler, errorHandler, NoAbort, 0, poll)
	res, err := future.Await(ctx, timeout)
	if err == nil {
		t.Errorf("Expected error but found %v", res)
	} else if !strings.Contains(err.Error(), "watchdog error") {
		t.Errorf("Expected fake watchdog error but was %v", err)
	}
}

func TestAsyncWatchdog(t *testing.T) {
	poll := time.Duration(1 * time.Millisecond)
	timeout := time.Duration(15 * time.Millisecond)
	ctx, cancel := context.WithTimeout(context.Background(), timeout)
	defer cancel()
	handler := func(ctx context.Context, payload interface{}) (interface{}, error) {
		time.Sleep(2 * time.Millisecond)
		return 10, nil
	}
	errorHandler := func(ctx context.Context, payload interface{}) error {
		return nil
	}

	future := ExecuteWatchdog(ctx, handler, errorHandler, NoAbort, 0, poll)
	res, err := future.Await(ctx, timeout)
	if err != nil {
		t.Errorf("Unexpected error %v", err)
	}
	if res != 10 {
		t.Errorf("Expected 10")
	}
}
