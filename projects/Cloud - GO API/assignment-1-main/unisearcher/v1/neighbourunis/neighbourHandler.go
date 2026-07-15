package neighbourunis

import (
	"assignment-1/unisearcher"
	"encoding/json"
	"fmt"
	"net/http"
	"strings"
)

func NeighbourUnisHandler(w http.ResponseWriter, r *http.Request) {
	// Handle GET requests
	switch r.Method {
	case http.MethodGet:
		// Parse country and university data from API endpoints
		countries, err := unisearcher.GetApiCountry(unisearcher.JS_COUNT_PATH)
		if err != nil {
			// Return 500 Internal Server Error if data cannot be parsed
			http.Error(w, "Failed to parse country data", http.StatusInternalServerError)
			return
		}
		universities, err := unisearcher.GetApiUni(unisearcher.JS_UNI_PATH)
		if err != nil {
			// Return 500 Internal Server Error if data cannot be parsed
			http.Error(w, "Failed to parse university data", http.StatusInternalServerError)
			return
		}

		// Extract country and university names from request path
		path := strings.TrimPrefix(r.URL.Path, unisearcher.NEIGBOURUNIS_PATH)
		if path == "" {
			// Return 404 Not Found if path is empty
			http.NotFound(w, r)
			return
		}
		parts := strings.Split(path, "/")
		if len(parts) < 2 {
			// Return 400 Bad Request if country and university names are not provided
			http.Error(w, "Country and university names are required", http.StatusBadRequest)
			return
		}
		countryName := strings.ToLower(parts[0])
		uniName := strings.ToLower(parts[1])

		// Find the country with the given name
		var country *unisearcher.Country
		for _, c := range countries {
			if strings.ToLower(c.Name.Common) == countryName {
				country = &c
				break
			}
		}
		if country == nil {
			// Return 400 Bad Request if country cannot be found
			http.Error(w, "Invalid country name", http.StatusBadRequest)
			return
		}

		// Find neighbouring countries
		borders := country.Borders
		var matchingCountry []unisearcher.Country
		for _, code := range borders {
			for _, c := range countries {
				if c.Is3code == code {
					matchingCountry = append(matchingCountry, c)
					break
				}
			}
		}

		// Find universities with matching names in neighbouring countries
		var matchingUniversities []unisearcher.University
		for _, u := range universities {
			if strings.HasPrefix(strings.ToLower(u.Name), uniName) {
				for _, c := range matchingCountry {
					if c.Iscode == u.Iscode {
						matchingUniversities = append(matchingUniversities, u)
						break
					}
				}
			}
		}

		if len(matchingUniversities) == 0 {
			// Return 404 Not Found if no matching universities were found
			http.NotFound(w, r)
			return
		}
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

		// Limit the number of results if specified in the query parameters
		limit := r.URL.Query().Get("limit")
		if limit != "" {
			var limitNum int
			_, err := fmt.Sscanf(limit, "%d", &limitNum)
			if err != nil {
				// Return 400 Bad Request if limit is not a valid number
				http.Error(w, "Limit must be a valid number", http.StatusBadRequest)
				return
			}
			if limitNum < len(matchingUniversities) {
				matchingUniversities = matchingUniversities[:limitNum]
			}
		}

		// Convert matching universities to JSON and write to response body
		jsonData, err := json.Marshal(matchingUniversities)
		if err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}

		// Set the Content-Type header and write the JSON data to the response body.
		w.Header().Set("Content-Type", "application/json")
		w.Write(jsonData)
	}
}
