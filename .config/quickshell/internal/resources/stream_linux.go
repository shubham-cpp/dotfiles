package resources

import (
	"bytes"
	"encoding/json"
	"errors"
	"io"
	"os"
	"time"

	"golang.org/x/sys/unix"
)

const maxFrame = 1024 * 1024

type monitorStream struct {
	monitor  *Monitor
	emit     func(any) error
	buffer   []byte
	paused   bool
	deadline time.Time
}

func (s *monitorStream) request(line []byte) error {
	var request map[string]json.RawMessage
	if !json.Valid(line) {
		return s.emit(invalidSelection())
	}
	// Valid non-object JSON is silently ignored, matching the existing reader.
	if json.Unmarshal(line, &request) != nil || request == nil {
		return nil
	}
	var action string
	_ = json.Unmarshal(request["action"], &action)
	switch action {
	case "pause":
		s.paused = string(request["paused"]) == "true"
		if !s.paused {
			s.monitor.resetCPU()
			s.deadline = s.monitor.now()
		}
	case "end":
		if err := s.emit(s.monitor.end(request)); err != nil {
			return err
		}
		return s.emit(s.monitor.message(s.monitor.now()))
	}
	return nil
}

func (s *monitorStream) consume(chunk []byte) (bool, error) {
	if len(chunk) == 0 || len(s.buffer)+len(chunk) > maxFrame {
		return false, nil
	}
	s.buffer = append(s.buffer, chunk...)
	for {
		index := bytes.IndexByte(s.buffer, '\n')
		if index < 0 {
			break
		}
		if err := s.request(s.buffer[:index]); err != nil {
			return false, err
		}
		s.buffer = s.buffer[index+1:]
	}
	if len(s.buffer) == 0 {
		s.buffer = nil
	}
	return true, nil
}

func (s *monitorStream) tick() error {
	if !s.paused && !s.monitor.now().Before(s.deadline) {
		if err := s.emit(s.monitor.sample()); err != nil {
			return err
		}
		s.deadline = s.monitor.now().Add(2 * time.Second)
	}
	return nil
}

// Run blocks on stdin or the next sample deadline. Pausing removes the deadline
// entirely; EOF ends the child. There are no polling subprocesses or reader
// goroutines which could outlive their input stream.
func Run(input *os.File, output io.Writer) error {
	p, err := newProcReader(desktopCatalog())
	if err != nil {
		return unavailable(err)
	}
	encoder := json.NewEncoder(output)
	s := &monitorStream{monitor: newMonitor(p), emit: encoder.Encode, deadline: time.Now()}
	fds := []unix.PollFd{{Fd: int32(input.Fd()), Events: unix.POLLIN}}
	chunk := make([]byte, 65536)
	for {
		timeout := -1
		if !s.paused {
			remaining := time.Until(s.deadline)
			timeout = max(0, int((remaining+time.Millisecond-1)/time.Millisecond))
		}
		count, err := unix.Poll(fds, timeout)
		if errors.Is(err, unix.EINTR) {
			continue
		}
		if err != nil {
			return err
		}
		if count > 0 {
			if fds[0].Revents&unix.POLLNVAL != 0 {
				return errors.New("invalid monitor input descriptor")
			}
			n, err := unix.Read(int(input.Fd()), chunk)
			if errors.Is(err, unix.EINTR) || errors.Is(err, unix.EAGAIN) {
				continue
			}
			if err != nil {
				return err
			}
			more, err := s.consume(chunk[:n])
			if err != nil || !more {
				return err
			}
		}
		if err := s.tick(); err != nil {
			return err
		}
	}
}
