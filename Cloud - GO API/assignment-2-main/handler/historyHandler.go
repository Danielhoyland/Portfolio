package handler

import (
	"assignment-2/helper"
	"encoding/json"
	"fmt"
	"net/http"
	"strings"
)

// HistoryHandler handles the API endpoint for historical country data
func HistoryHandler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("content-type", "application/json")

	// Extract the URL path parts
	parts := strings.Split(r.URL.Path, "/")
	// Get the ISO code from the URL path, if provided
	var tags []string
	if len(parts) > 5 && parts[5] != "" { // check if an ISO code is provided in the URL path
		tags = []string{parts[5]}
	} else {
		tags = []string{"all"} // return data for all countries
	}
	histCountry(w, r, tags[0])
}

// histCountry returns the historical data for the specified country or for all countries
func histCountry(w http.ResponseWriter, r *http.Request, iso string) {
	countries := []helper.Country{}

	// Get the begin and end years from the URL query parameters
	begin := r.URL.Query().Get("begin")
	end := r.URL.Query().Get("end")
	if begin == "" && end == "" {
		countries = helper.GetCountry(iso, "0", "100000") // Get all historical data
	} else {
		countries = helper.GetCountry(iso, begin, end) // Get historical data within a specified time range
	}

	// Update webhooks for each country
	for _, val := range countries {
		NotifCountryCall(val.Iso, "GET")
	}

	if countries == nil {
		http.Error(w, "Couldnt do the request!", http.StatusBadRequest)
		return
	}

	// Marshal the country data to JSON and write to the response writer
	encoded, err := json.Marshal(countries)
	if err != nil {
		http.Error(w, "Could not marshal!", http.StatusInternalServerError)
		return
	}
	fmt.Fprint(w, string(encoded))
}
