package handler

import (
	"assignment-2/helper"
	"fmt"
	"net/http"
)

// default handler because it was getting annoying having to go copy the constants to get the real path
func DefaultHandler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("content-type", "html")
	fmt.Fprint(w, "No functionality for default handler!\n\n")

	fmt.Fprint(w, "try using one of these:\n")

	fmt.Fprint(w, r.URL.Host+helper.CURRENT+"\n")
	fmt.Fprint(w, r.URL.Host+helper.HISTORY+"\n")
	fmt.Fprint(w, r.URL.Host+helper.NOTIFICATIONS+"\n")
	fmt.Fprint(w, r.URL.Host+helper.STATUS+"\n")

}
