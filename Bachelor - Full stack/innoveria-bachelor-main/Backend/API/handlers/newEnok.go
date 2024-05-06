package handlers

import (
	"API/other"
	"encoding/json"
	"log"
	"net/http"
)

func NewEnoek(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Access-Control-Allow-Headers", "Content-Type")
	w.Header().Set("Access-Control-Allow-Origin", ""+other.WebsiteURL)
	w.Header().Set("Access-Control-Allow-Methods", "POST, PUT, DELETE, OPTIONS")
	if r.Method == "POST" {
		decoder := json.NewDecoder(r.Body)
		//creates an anonymous struct to reduce clutter in structs file
		data := struct {
			Enoek        other.EnokSuggestionMeasures `json:"enoek"`
			SessionToken string                       `json:"sessionToken"`
			ProcessName  string                       `json:"process"`
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
		if permissionLevel > 2 {
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

		dbProcess := other.Processes{
			Name:      data.ProcessName,
			CompanyID: CompID,
		}

		process, err := other.GetProcessIDByName(dbProcess)
		if err != nil {
			log.Println(err.Error())
			w.WriteHeader(http.StatusBadRequest)
			return
		}

		dbEnoek := other.EnokSuggestionMeasures{
			Header:      data.Enoek.Header,
			StartDate:   data.Enoek.StartDate,
			StopDate:    data.Enoek.StopDate,
			Description: data.Enoek.Description,
			ProcessID:   process,
		}
		//calls function to add building
		err = other.InsertEnoek(data.SessionToken, dbEnoek)
		if err != nil {
			println(err.Error())
			w.WriteHeader(http.StatusInternalServerError)
			return
		}

	} else if r.Method == http.MethodPut {
		decoder := json.NewDecoder(r.Body)
		//creates an anonymous struct to reduce clutter in structs file
		data := struct {
			ID           int    `json:"id"`
			Bool         bool   `json:"bool"`
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
		if permissionLevel > 1 {
			w.WriteHeader(http.StatusUnauthorized)
			return
		}

		//calls function to add building
		err = other.EnoekJudgement(data.ID, data.Bool)
		if err != nil {
			println(err.Error())
			w.WriteHeader(http.StatusInternalServerError)
			return
		}
	} else if r.Method == http.MethodDelete {
		decoder := json.NewDecoder(r.Body)
		//creates an anonymous struct to reduce clutter in structs file
		data := struct {
			ID           int    `json:"id"`
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
		if permissionLevel > 1 {
			w.WriteHeader(http.StatusUnauthorized)
			return
		}
		//calls function to add building
		err = other.DeleteEnoek(data.ID)
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
