package handler

import (
	"assignment-2/helper"
	"fmt"
	"net/http"
	"os"
	"path/filepath"
)

func StatusHandler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("content-type", "application/json")

	// Send a dummy request to get the status of the restcountries API
	res, _ := http.Get(helper.RESTC + "/alpha/NOR")

	// Print the status of the restcountries API
	fmt.Fprint(w, "status of restcountries API: "+res.Status+"\n")
	// Get the current working directory of the project
	wd, err := os.Getwd()
	if err == nil {
		// Construct the path to the CSV file using the project directory
		var energyFilePath = ""
		if filepath.Base(wd) == "assignment-2" {
			energyFilePath = filepath.Join(wd, helper.ENERGY)
		} else {
			energyFilePath = filepath.Join(filepath.Dir(wd), helper.ENERGY)
		}
		// Check if energy data file exists
		_, err := os.Open(energyFilePath)
		if err != nil {
			fmt.Fprintln(w, "Couldnt find energy data!")
		} else {
			fmt.Fprintln(w, "Energy data located!")
		}
	} else {
		fmt.Fprintln(w, "Couldnt find energy data!")
	}

	// Print the uptime of the application
	fmt.Fprintf(w, "Uptime: %d seconds\n", int(helper.Uptime().Seconds()))
}
