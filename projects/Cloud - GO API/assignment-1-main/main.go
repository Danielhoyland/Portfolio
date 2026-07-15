package main

import (
	"assignment-1/unisearcher"
	"assignment-1/unisearcher/v1/diag"
	"assignment-1/unisearcher/v1/neighbourunis"
	"assignment-1/unisearcher/v1/uniinfo"
	"log"
	"net/http"
	"os"
)

func main() {

	// Define port
	port := os.Getenv("PORT")
	if port == "" {
		log.Println("$PORT has not been set. Default: 8080")
		port = "8080"
	}
	http.HandleFunc(unisearcher.DEFAULT_PATH1, unisearcher.EmptyHandler)
	http.HandleFunc(unisearcher.DEFAULT_PATH2, unisearcher.EmptyHandler)
	http.HandleFunc(unisearcher.DEFAULT_PATH3, unisearcher.EmptyHandler)
	http.HandleFunc(unisearcher.UNI_PATH, uniinfo.UniHandler)
	http.HandleFunc(unisearcher.DIAG_PATH, diag.StubHandlerDiag)
	http.HandleFunc(unisearcher.NEIGBOURUNIS_PATH, neighbourunis.NeighbourUnisHandler)

	log.Println("Running on port", port)

	err := http.ListenAndServe(":"+port, nil)
	if err != nil {
		log.Fatal(err.Error())
	}

}
