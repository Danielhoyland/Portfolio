package uniinfo

import (
	"assignment-1/unisearcher"
	"encoding/json"
	"log"
	"net/http"
	"strings"
)

func UniHandler(w http.ResponseWriter, r *http.Request) {
	switch r.Method {
	case http.MethodGet:
		// Log that a GET request has been received for the University stub handler.
		log.Println("Received " + r.Method + " request on University stub handler. Returning mocked information.")

		// Retrieve the data for universities and countries.
		countries, err := unisearcher.GetApiCountry(unisearcher.JS_COUNT_PATH)
		if err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}
		universities, err := unisearcher.GetApiUni(unisearcher.JS_UNI_PATH)
		if err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}

		// Extract the name of the university from the request path.
		name := strings.TrimSuffix(strings.ToLower(r.URL.Path[len(unisearcher.UNI_PATH):]), "/")

		// Find all universities that match the given name.
		var matchingUniversities []unisearcher.University
		for _, u := range universities {
			if strings.HasPrefix(strings.ToLower(u.Name), name) {
				matchingUniversities = append(matchingUniversities, u)
			}
		}

		if len(matchingUniversities) == 0 {
			// If no matching universities were found, return a 404 Not Found error.
			http.NotFound(w, r)
			return
		}

		// For each matching university, find the country that matches its alpha-2 code.
		for i, u := range matchingUniversities {
			for _, c := range countries {
				if c.Iscode == u.Iscode {
					// Add the languages and map information from the country to the university.
					matchingUniversities[i].Languages = c.Languages
					for k, v := range c.Map {
						if k == "openStreetMaps" {
							matchingUniversities[i].OpenStreetMap = v
						}
					}
					break
				}
			}
		}

		// Marshal the university information into JSON.
		jsonData, err := json.Marshal(matchingUniversities)
		if err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}

		// Set the Content-Type header and write the JSON data to the response body.
		w.Header().Set("Content-Type", "application/json")
		w.Write(jsonData)
		break
	default:
		// If the request method is not GET, return a 501 Not Implemented error.
		http.Error(w, "Method not supported", http.StatusNotImplemented)
	}
}
