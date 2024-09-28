package handler

import (
	"assignment-2/helper"
	"encoding/json"
	"fmt"
	"net/http"
	"sort"
	"strings"
)

func CurrentHandler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("content-type", "application/json")

	// Split the URL path to determine if a specific country was requested
	parts := strings.Split(r.URL.Path, "/")
	if len(parts) > 5 && parts[5] != "" {
		oneCountry(w, r, parts[5])
	} else {
		allCountries(w, r)
	}
}

// allCountries fetches data for all countries and sends it to the client
func allCountries(w http.ResponseWriter, r *http.Request) {
	// Get data for all countries
	countries := helper.GetCountry("allCurrent", "", "")
	if countries == nil {
		http.Error(w, "Couldnt do the request!", http.StatusBadRequest)
		return
	}

	// Filter countries by the highest year
	filtered := make(map[string]helper.Country)
	for _, c := range countries {
		if _, ok := filtered[c.Iso]; !ok || filtered[c.Iso].Year < c.Year {
			filtered[c.Iso] = c
		}
	}
	result := make([]helper.Country, 0, len(filtered))
	for _, c := range filtered {
		result = append(result, c)
	}

	// Sort countries by name
	sort.Slice(result, func(i, j int) bool {
		return result[i].Name < result[j].Name
	})

	// Trigger webhooks for all countries
	for i := range result {
		NotifCountryCall(result[i].Iso, "GET")
	}

	// Encode the resulting data as JSON and send it to the client
	encoded, err := json.Marshal(result)
	if err != nil {
		http.Error(w, "Could not marshal!", http.StatusInternalServerError)
		return
	}
	fmt.Fprint(w, string(encoded))
}

// oneCountry fetches data for a single country and sends it to the client
func oneCountry(w http.ResponseWriter, r *http.Request, iso string) {
	// Get data for the requested country
	countries := helper.GetCountry(iso, "2021", "2022")
	n := r.URL.Query().Get("neighbours")
	if countries == nil {
		http.Error(w, "Couldnt do the request!", http.StatusBadRequest)
		return
	}

	// Trigger webhooks for the requested country
	for i := range countries {
		NotifCountryCall(countries[i].Iso, "GET")
	}

	// If the 'neighbours' query parameter is set to 'true', fetch data for neighboring countries
	if strings.ToLower(n) == "true" && len(countries) == 1 {
		for _, b := range countries[0].Borders {
			countries = append(countries, helper.GetCountry(b, "2021", "2022")[0])
		}
	}

	// Encode the resulting data as JSON and send it to the client
	encoded, err := json.Marshal(countries)
	if err != nil {
		http.Error(w, "Could not marshal!", http.StatusInternalServerError)
		return
	}
	fmt.Fprint(w, string(encoded))
}
