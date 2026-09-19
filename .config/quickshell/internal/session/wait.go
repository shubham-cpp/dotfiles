package session

import (
	"context"
	"errors"
	"os/exec"
	"path/filepath"
	"strings"
	"time"
)

var errNotSecure = errors.New("compositor lock was not confirmed")

// WaitLocked addresses the existing shell and returns only for its secure status.
// The shell path is explicit because installed binaries live outside scripts/.
func WaitLocked(ctx context.Context, shell string) error {
	return waitLocked(ctx, shell, func(ctx context.Context, shell, action string) (string, error) {
		cmd := exec.CommandContext(ctx, "qs", "-p", shell, "ipc", "call", "lock", action)
		var output boundedOutput
		cmd.Stdout = &output
		err := cmd.Run()
		if ctx.Err() != nil {
			return "", ctx.Err()
		}
		return output.String(), err
	})
}

type boundedOutput struct{ strings.Builder }

func (w *boundedOutput) Write(data []byte) (int, error) {
	if w.Len()+len(data) > maxFrame {
		return 0, errors.New("oversized lock status")
	}
	return w.Builder.Write(data)
}

func waitLocked(ctx context.Context, shell string, command func(context.Context, string, string) (string, error)) error {
	if shell == "" {
		return errors.New("missing shell path")
	}
	shell, err := filepath.Abs(shell)
	if err != nil {
		return err
	}
	ctx, cancel := context.WithTimeout(ctx, 5*time.Second)
	defer cancel()
	activateCtx, stop := context.WithTimeout(ctx, 2*time.Second)
	_, err = command(activateCtx, shell, "activate")
	stop()
	if err != nil {
		return errNotSecure
	}
	for ctx.Err() == nil {
		statusCtx, stop := context.WithTimeout(ctx, 500*time.Millisecond)
		output, err := command(statusCtx, shell, "status")
		stop()
		if err == nil && strings.TrimSpace(output) == "secure" {
			return nil
		}
		if err != nil && !errors.Is(err, context.DeadlineExceeded) {
			return errNotSecure
		}
		timer := time.NewTimer(100 * time.Millisecond)
		select {
		case <-ctx.Done():
			timer.Stop()
			return errNotSecure
		case <-timer.C:
		}
	}
	return errNotSecure
}
