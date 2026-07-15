package main

import (
	"assignment-2/handler"
	"assignment-2/helper"
	"log"
	"net/http"
	"os"
)

func main() {
	helper.Init()

	port := os.Getenv("PORT")
	if port == "" {
		log.Println("Port not found, using default, 8080...")
		port = "8080"
	}
	//sends the user to appropriate request
	http.HandleFunc("/", handler.DefaultHandler)
	http.HandleFunc(helper.STATUS, handler.StatusHandler)
	http.HandleFunc(helper.NOTIFICATIONS, handler.NotificationHandler)
	http.HandleFunc(helper.HISTORY, handler.HistoryHandler)
	http.HandleFunc(helper.CURRENT, handler.CurrentHandler)

	log.Println("Starting server on port " + port + " ...")
	log.Fatal(http.ListenAndServe(":"+port, nil))

}
