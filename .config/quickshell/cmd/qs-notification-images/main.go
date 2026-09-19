package main

import (
	"encoding/json"
	"os"
	"quickshell.local/helpers/internal/notificationimages"
)

func main() {
	if len(os.Args) == 3 {
		result, err := notificationimages.Update(os.Args[1], []byte(os.Args[2]))
		if err == nil {
			if json.NewEncoder(os.Stdout).Encode(result) == nil {
				return
			}
		}
	}
	_ = json.NewEncoder(os.Stdout).Encode(map[string]string{"error": "Notification image cache unavailable"})
	os.Exit(1)
}
