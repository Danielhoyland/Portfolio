package unisearcher

import (
	"fmt"
	"net/http"
)

func EmptyHandler(w http.ResponseWriter, r *http.Request) {

	// Ensure interpretation as HTML by client (browser)
	w.Header().Set("content-type", "text/html")

	// Offer information for redirection to paths
	output := "This service does not provide any functionality on root path level. Please use paths <a href=\"" +
		UNI_PATH + "\">" + UNI_PATH + "</a> or <a href=\"" + NEIGBOURUNIS_PATH +
		"\">" + NEIGBOURUNIS_PATH + "</a> or <a href=\"" + DIAG_PATH +
		"\">" + DIAG_PATH + "</a>."

	// Write output to client
	_, err := fmt.Fprintf(w, "%v", output)

	// Deal with error if any
	if err != nil {
		http.Error(w, "Error when returning output", http.StatusInternalServerError)
	}

}
