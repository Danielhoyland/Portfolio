package handlers

import (
	"API/other"
	"encoding/json"
	"log"
	"net/http"
)

func NewBuilding(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Access-Control-Allow-Headers", "Content-Type")
	w.Header().Set("Access-Control-Allow-Origin", ""+other.WebsiteURL)
	w.Header().Set("Access-Control-Allow-Methods", "POST, PUT, DELETE, OPTIONS")
	if r.Method == "POST" {
		decoder := json.NewDecoder(r.Body)
		//creates an anonymous struct to reduce clutter in structs file
		data := struct {
			BuildingName string `json:"building_name"`
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

		//calls function to add building
		err = other.InsertBuilding(data.SessionToken, data.BuildingName)
		if err != nil {
			println(err.Error())
			w.WriteHeader(http.StatusInternalServerError)
			return
		}

	} else if r.Method == http.MethodPut {
		decoder := json.NewDecoder(r.Body)
		//creates an anonymous struct to reduce clutter in structs file
		data := struct {
			BuildingName1 string `json:"old_building_name"`
			BuildingName2 string `json:"new_building_name"`
			SessionToken  string `json:"sessionToken"`
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
		//get the building id
		buildingId, err := other.GetBuildingIdByName(data.BuildingName1, CompID)
		if err != nil {
			log.Println(err.Error())
			w.WriteHeader(http.StatusBadRequest)
			return
		}
		//calls function to edit building
		err = other.EditBuilding(buildingId, data.BuildingName2)
		if err != nil {
			println(err.Error())
			w.WriteHeader(http.StatusInternalServerError)
			return
		}
	} else if r.Method == http.MethodDelete {
		decoder := json.NewDecoder(r.Body)
		//creates an anonymous struct to reduce clutter in structs file
		data := struct {
			BuildingName string `json:"building_name"`
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
		//get the building id
		buildingId, err := other.GetBuildingIdByName(data.BuildingName, CompID)
		if err != nil {
			log.Println(err.Error())
			w.WriteHeader(http.StatusBadRequest)
			return
		}
		//calls function to add building
		err = other.DeleteBuilding(buildingId)
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
