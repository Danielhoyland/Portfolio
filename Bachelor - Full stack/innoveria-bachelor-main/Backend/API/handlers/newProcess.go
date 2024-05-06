package handlers

import (
	"API/other"
	"encoding/json"
	"log"
	"net/http"
)

func NewProcess(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Access-Control-Allow-Headers", "Content-Type")
	w.Header().Set("Access-Control-Allow-Origin", ""+other.WebsiteURL)
	w.Header().Set("Access-Control-Allow-Methods", "POST, PUT, DELETE, OPTIONS")
	if r.Method == "POST" {
		decoder := json.NewDecoder(r.Body)
		//creates an anonymous struct to reduce clutter in structs file
		data := struct {
			ProcessName  string `json:"process_name"`
			Description  string `json:"description"`
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
		//verify that the user making the request is allowed
		permissionLevel, err := other.GetPermissionFromToken(data.SessionToken)
		if err != nil {
			log.Println(err.Error())
			w.WriteHeader(http.StatusInternalServerError)
			return
		}
		if permissionLevel != 0 {
			w.WriteHeader(http.StatusUnauthorized)
			return
		}
		process := other.Processes{
			Name:        data.ProcessName,
			Description: data.Description,
		}
		//calls function to add building
		err = other.InsertProcess(data.SessionToken, process)
		if err != nil {
			println(err.Error())
			w.WriteHeader(http.StatusInternalServerError)
			return
		}

	} else if r.Method == http.MethodPut {
		decoder := json.NewDecoder(r.Body)
		//creates an anonymous struct to reduce clutter in structs file
		data := struct {
			ProcessName1 string `json:"old_process_name"`
			ProcessName2 string `json:"new_process_name"`
			Description1 string `json:"old_description"`
			Description2 string `json:"new_description"`
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
		//verify that the user making the request is allowed
		permissionLevel, err := other.GetPermissionFromToken(data.SessionToken)
		if err != nil {
			log.Println(err.Error())
			w.WriteHeader(http.StatusInternalServerError)
			return
		}
		if permissionLevel != 0 {
			w.WriteHeader(http.StatusUnauthorized)
			return
		}
		UserID, err := other.GetUserIDFromToken(data.SessionToken)
		if err != nil {
			log.Println(err.Error())
			w.WriteHeader(http.StatusInternalServerError)
			return
		}
		CompID, err := other.GetCompanyFromUserID(UserID)
		if err != nil {
			log.Println(err.Error())
			w.WriteHeader(http.StatusInternalServerError)
			return
		}
		oldProcess := other.Processes{
			Name:        data.ProcessName1,
			Description: data.Description1,
			CompanyID:   CompID,
		}
		processId, err := other.GetProcessIDByName(oldProcess)
		if err != nil {
			log.Println(err.Error())
			w.WriteHeader(http.StatusBadRequest)
			return
		}
		newProcess := other.Processes{
			ID:          processId,
			Name:        data.ProcessName2,
			Description: data.Description2,
		}
		//calls function to add building
		err = other.EditProcess(newProcess)
		if err != nil {
			println(err.Error())
			w.WriteHeader(http.StatusInternalServerError)
			return
		}
	} else if r.Method == http.MethodDelete {
		decoder := json.NewDecoder(r.Body)
		//creates an anonymous struct to reduce clutter in structs file
		data := struct {
			ProcessName  string `json:"process_name"`
			Description  string `json:"description"`
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
		//verify that the user making the request is allowed
		permissionLevel, err := other.GetPermissionFromToken(data.SessionToken)
		if err != nil {
			log.Println(err.Error())
			w.WriteHeader(http.StatusInternalServerError)
			return
		}
		if permissionLevel != 0 {
			w.WriteHeader(http.StatusUnauthorized)
			return
		}
		UserID, err := other.GetUserIDFromToken(data.SessionToken)
		if err != nil {
			log.Println(err.Error())
			w.WriteHeader(http.StatusInternalServerError)
			return
		}
		CompID, err := other.GetCompanyFromUserID(UserID)
		if err != nil {
			log.Println(err.Error())
			w.WriteHeader(http.StatusInternalServerError)
			return
		}
		oldProcess := other.Processes{
			Name:        data.ProcessName,
			Description: data.Description,
			CompanyID:   CompID,
		}
		processId, err := other.GetProcessIDByName(oldProcess)
		if err != nil {
			log.Println(err.Error())
			w.WriteHeader(http.StatusBadRequest)
			return
		}
		//calls function to add building
		err = other.DeleteProcess(processId)
		if err != nil {
			println(err.Error())
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
	w.Header().Set("Access-Control-Allow-Methods", "POST, PUT, DELETE, OPTIONS")
	if r.Method == "POST" {

	} else if r.Method == http.MethodPut {
		decoder := json.NewDecoder(r.Body)

		type RequestBody struct {
			Data []struct {
				AddOrNot   bool   `json:"added"`
				ProcessId  int    `json:"processId"`
				MachineEUI string `json:"eui"`
			} `json:"data"`
			SessionToken string `json:"sessionToken"`
		}

		var requestBody RequestBody
		err := decoder.Decode(&requestBody)
		if err != nil {
			println(err.Error())
			return
		}
		log.Println(requestBody.SessionToken)
		valid, err := other.IsTokenStillActive(requestBody.SessionToken)
		if !valid {
			log.Println("Session token expired or not valid!")
			w.WriteHeader(http.StatusUnauthorized)
			if err != nil {
				log.Println(err.Error())
			}
			return
		}
		//verify that the user making the request is allowed
		permissionLevel, err := other.GetPermissionFromToken(requestBody.SessionToken)
		if err != nil {
			log.Println(err.Error())
			w.WriteHeader(http.StatusInternalServerError)
			return
		}
		if permissionLevel != 0 {
			w.WriteHeader(http.StatusUnauthorized)
			return
		}
		for _, item := range requestBody.Data {
			//calls function to add building
			err = other.EditSensorToProcess(item.AddOrNot, item.ProcessId, item.MachineEUI)
			if err != nil {
				println(err.Error())
				w.WriteHeader(http.StatusInternalServerError)
				return
			}
		}
	} else if r.Method == http.MethodDelete {
		decoder := json.NewDecoder(r.Body)
		//creates an anonymous struct to reduce clutter in structs file
		data := struct {
			ProcessName  string `json:"process_name"`
			Description  string `json:"description"`
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
		//verify that the user making the request is allowed
		permissionLevel, err := other.GetPermissionFromToken(data.SessionToken)
		if err != nil {
			log.Println(err.Error())
			w.WriteHeader(http.StatusInternalServerError)
			return
		}
		if permissionLevel != 0 {
			w.WriteHeader(http.StatusUnauthorized)
			return
		}
		UserID, err := other.GetUserIDFromToken(data.SessionToken)
		if err != nil {
			log.Println(err.Error())
			w.WriteHeader(http.StatusInternalServerError)
			return
		}
		CompID, err := other.GetCompanyFromUserID(UserID)
		if err != nil {
			log.Println(err.Error())
			w.WriteHeader(http.StatusInternalServerError)
			return
		}
		oldProcess := other.Processes{
			Name:        data.ProcessName,
			Description: data.Description,
			CompanyID:   CompID,
		}
		processId, err := other.GetProcessIDByName(oldProcess)
		if err != nil {
			log.Println(err.Error())
			w.WriteHeader(http.StatusBadRequest)
			return
		}
		//calls function to add building
		err = other.DeleteProcess(processId)
		if err != nil {
			println(err.Error())
			w.WriteHeader(http.StatusInternalServerError)
			return
		}
	} else if r.Method == http.MethodOptions {

	} else {
		w.WriteHeader(http.StatusMethodNotAllowed)
	}
}
