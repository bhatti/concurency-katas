package async

import (
	"context" //"strings"
	"errors"
	"strings"
	"testing"
	"time"
)

func TestAsyncPollingWithTimeout(t *testing.T) {
	poll := time.Duration(1 * time.Millisecond)
	timeout := time.Duration(5 * time.Millisecond)
	ctx, cancel := context.WithTimeout(context.Background(), timeout)
	defer cancel()
	val := 0
	handler := func(ctx context.Context, payload interface{}) (bool, interface{}, error) {
		time.Sleep(1 * time.Millisecond)
		val += 1
		return val >= 10, val, nil
	}

	future := ExecutePolling(ctx, handler, NoAbort, 0, poll)
	_, err := future.Await(ctx, timeout)
	if err == nil {
		t.Errorf("Expected error")
	} else if !strings.Contains(err.Error(), "deadline exceeded") {
		t.Errorf("Expected deadline exceeded but was %v", err)
	}
}

func TestAsyncPollingWithFailure(t *testing.T) {
	poll := time.Duration(1 * time.Millisecond)
	timeout := time.Duration(10 * time.Millisecond)
	ctx, cancel := context.WithTimeout(context.Background(), timeout)
	defer cancel()
	val := 0
	handler := func(ctx context.Context, payload interface{}) (bool, interface{}, error) {
		time.Sleep(1 * time.Millisecond)
		if val > 1 {
			return false, nil, errors.New("fake poll error")
		}
		val += 1
		return val >= 10, val, nil
	}

	future := ExecutePolling(ctx, handler, NoAbort, 0, poll)
	_, err := future.Await(ctx, timeout)
	if err == nil {
		t.Errorf("Expected error")
	} else if !strings.Contains(err.Error(), "fake poll") {
		t.Errorf("Expected fake poll error but was %v", err)
	}
}

func TestAsyncPolling(t *testing.T) {
	poll := time.Duration(1 * time.Millisecond)
	timeout := time.Duration(15 * time.Millisecond)
	ctx, cancel := context.WithTimeout(context.Background(), timeout)
	defer cancel()
	val := 0
	handler := func(ctx context.Context, payload interface{}) (bool, interface{}, error) {
		time.Sleep(1 * time.Millisecond)
		val += 1
		return val >= 5, val, nil
	}

	future := ExecutePolling(ctx, handler, NoAbort, 0, poll)
	res, err := future.Await(ctx, timeout)
	if err != nil {
		t.Errorf("Unexpected error %v", err)
	}
	if res != 5 {
		t.Errorf("Expected count to 5")
	}
}
