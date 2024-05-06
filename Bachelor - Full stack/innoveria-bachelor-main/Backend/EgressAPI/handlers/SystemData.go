package handlers

import (
	"EgressAPI/other"
	"encoding/json"
	"log"
	"net/http"
)

func BuildDep(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Access-Control-Allow-Headers", "Content-Type")
	w.Header().Set("Access-Control-Allow-Origin", ""+other.WebsiteURL)
	w.Header().Set("Access-Control-Allow-Methods", "OPTIONS, POST")
	if r.Method == http.MethodPost {
		token, err := other.ValidateRequest(r.Body)
		if err != nil {
			log.Println(err.Error())
			w.WriteHeader(http.StatusInternalServerError)
			return
		}

		buildingDep, err := other.GetBuildingsAndDepartments(token)
		if err != nil {
			log.Println(err.Error())
			return
		}

		// Define a struct to hold your data
		type responseData struct {
			BuildingDep map[string][]other.Department `json:"buildingDep"`
			Process     []other.Processes             `json:"process"`
		}

		buildingDepJSON := make(map[string][]other.Department)
		for key, value := range buildingDep {
			departments := make([]other.Department, len(value))
			for i, dept := range value {
				departments[i] = other.Department{Name: dept.Name}
			}
			buildingDepJSON[key.Name] = departments
		}

		// Prepare the response data
		response := responseData{
			BuildingDep: buildingDepJSON,
		}

		encoder := json.NewEncoder(w)
		err = encoder.Encode(response)
		if err != nil {
			log.Println(err.Error())
			w.WriteHeader(http.StatusInternalServerError)
			return
		}
	} else if r.Method == http.MethodOptions {

	} else {
		w.WriteHeader(http.StatusMethodNotAllowed)
	}
}
func Process(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Access-Control-Allow-Headers", "Content-Type")
	w.Header().Set("Access-Control-Allow-Origin", ""+other.WebsiteURL)
	w.Header().Set("Access-Control-Allow-Methods", "OPTIONS, POST")
	if r.Method == http.MethodPost {
		token, err := other.ValidateRequest(r.Body)
		if err != nil {
			log.Println(err.Error())
			w.WriteHeader(http.StatusInternalServerError)
			return
		}

		process, err := other.GetProcess(token)
		if err != nil {
			log.Println(err.Error())
			return
		}

		// Define a struct to hold your data
		type responseData struct {
			Process []other.Processes `json:"process"`
		}

		// Prepare the response data
		response := responseData{
			Process: process,
		}

		encoder := json.NewEncoder(w)
		err = encoder.Encode(response)
		if err != nil {
			log.Println(err.Error())
			w.WriteHeader(http.StatusInternalServerError)
			return
		}
	} else if r.Method == http.MethodOptions {

	} else {
		w.WriteHeader(http.StatusMethodNotAllowed)
	}
}

func ProcessMachine(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Access-Control-Allow-Headers", "Content-Type")
	w.Header().Set("Access-Control-Allow-Origin", ""+other.WebsiteURL)
	w.Header().Set("Access-Control-Allow-Methods", "OPTIONS, POST")
	if r.Method == http.MethodPost {
		decoder := json.NewDecoder(r.Body)
		//creates an anonymous struct to reduce clutter in structs file
		data := struct {
			Process      int    `json:"process"`
			SessionToken string `json:"sessionToken"`
		}{}
		err := decoder.Decode(&data)
		if err != nil {
			println(err.Error())
			return
		}
		valid, err := other.IsTokenStillActive(data.SessionToken)
		if !valid {
			log.Println("Session token expired or not valid!")
			w.WriteHeader(http.StatusUnauthorized)
			if err != nil {
				log.Println(err.Error())
			}
			return
		}
		processMachines, machines, err := other.GetMachinesForProcess(data.SessionToken, data.Process)
		if err != nil {
			log.Println(err.Error())
			return
		}

		// Define a struct to hold your data
		type responseData struct {
			AddedOrNot []bool                `json:"added"`
			Machines   []other.SimpleMachine `json:"machines"`
		}

		// Initialize the responseData struct
		var response responseData

		// Loop over machines
		for _, machine := range machines {
			var added = false
			// Check if the machine is in processMachines
			for _, processMachine := range processMachines {
				if processMachine.MachineID == machine.EUI {
					added = true
					break
				}
			}
			// Append the result to the responseData slice
			response.AddedOrNot = append(response.AddedOrNot, added)
		}
		response.Machines = machines
		encoder := json.NewEncoder(w)
		err = encoder.Encode(response)
		if err != nil {
			log.Println(err.Error())
			w.WriteHeader(http.StatusInternalServerError)
			return
		}
	} else if r.Method == http.MethodOptions {

	} else {
		w.WriteHeader(http.StatusMethodNotAllowed)
	}
}
