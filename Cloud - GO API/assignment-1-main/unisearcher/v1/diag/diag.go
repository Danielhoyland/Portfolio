package diag

import (
	"assignment-1/unisearcher"
	"encoding/json"
	"net/http"
	"os"
	"time"
)

func timeSince() int {
	stat, err := os.Stat(os.Args[0])
	if err != nil {
		panic(err)
	}
	lastRestart := stat.ModTime()
	uptime := time.Since(lastRestart)
	secondsSinceRestart := int(uptime.Seconds())
	return secondsSinceRestart
}

func StubHandlerDiag(w http.ResponseWriter, r *http.Request) {
	// Get uptime since last restart
	secondsSinceRestart := timeSince()

	// Check the status of the universities API
	universitiesStatus := http.StatusInternalServerError
	resp, err := http.Get(unisearcher.JS_UNI_PATH)
	if err == nil {
		universitiesStatus = resp.StatusCode
		defer resp.Body.Close()
	}

	// Check the status of the countries API
	countriesStatus := http.StatusInternalServerError
	resp, err = http.Get(unisearcher.JS_COUNT_PATH)
	if err == nil {
		countriesStatus = resp.StatusCode
		defer resp.Body.Close()
	}

	// Set up response body with diagnostic information
	response := map[string]interface{}{
		"universitiesapi": universitiesStatus,
		"countriesapi":    countriesStatus,
		"version":         "v1",
		"uptime":          secondsSinceRestart,
	}

	// Marshal the response to JSON
	body, err := json.Marshal(response)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	// Set response headers
	w.Header().Set("Content-Type", "application/json")

	// Write response body to client
	w.Write(body)
}
