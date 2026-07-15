package handlers

import (
	"API/other"
	"encoding/json"
	"golang.org/x/crypto/bcrypt"
	"log"
	"net/http"
)

func NewUser(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Access-Control-Allow-Headers", "Content-Type")
	w.Header().Set("Access-Control-Allow-Origin", ""+other.WebsiteURL)
	w.Header().Set("Access-Control-Allow-Methods", "POST, PUT, DELETE, OPTIONS")
	if r.Method == "POST" {
		//decode all required json data
		userData := struct {
			SessionToken string      `json:"sessionToken"`
			User         other.Users `json:"userData"`
		}{}
		decoder := json.NewDecoder(r.Body)
		err := decoder.Decode(&userData)
		if err != nil {
			log.Println(err.Error()) //decode fail
			w.WriteHeader(http.StatusBadRequest)
			return
		}

		//verify that the user making the request is allowed
		permissionLevel, err := other.GetPermissionFromToken(userData.SessionToken)
		if err != nil {
			log.Println(err.Error())
			w.WriteHeader(http.StatusInternalServerError)
			return
		}
		if permissionLevel != 0 || userData.User.Permission > 3 || userData.User.Permission < 0 {
			w.WriteHeader(http.StatusUnauthorized)
			return
		}

		//encrypt password using bcrypt, with a cost of 14
		Hash, err := bcrypt.GenerateFromPassword([]byte(userData.User.Password), 14)
		if err != nil {
			log.Println(err.Error())
			w.WriteHeader(http.StatusInternalServerError)
			return
		}
		err = other.InsertUser(userData.SessionToken, userData.User.Email, userData.User.FirstName, userData.User.LastName, string(Hash), userData.User.Permission)
	} else if r.Method == http.MethodPut {
		//decode all required json data
		userData := struct {
			SessionToken string      `json:"sessionToken"`
			User         other.Users `json:"userData"`
		}{}
		decoder := json.NewDecoder(r.Body)
		err := decoder.Decode(&userData)
		if err != nil {
			log.Println(err.Error()) //decode fail
			w.WriteHeader(http.StatusBadRequest)
			return
		}

		//verify that the user making the request is allowed
		permissionLevel, err := other.GetPermissionFromToken(userData.SessionToken)
		if err != nil {
			log.Println(err.Error())
			w.WriteHeader(http.StatusInternalServerError)
			return
		}
		if permissionLevel != 0 {
			w.WriteHeader(http.StatusUnauthorized)
			return
		}

		//encrypt password using bcrypt, with a cost of 14
		Hash, err := bcrypt.GenerateFromPassword([]byte(userData.User.Password), 14)
		if err != nil {
			log.Println(err.Error())
			w.WriteHeader(http.StatusInternalServerError)
			return
		}
		err = other.EditUser(userData.User.ID, userData.User.Email, userData.User.FirstName, userData.User.LastName, string(Hash), userData.User.Permission)
		if err != nil {
			log.Println(err.Error())
			w.WriteHeader(http.StatusInternalServerError)
			return
		}
	} else if r.Method == http.MethodDelete {
		userData := struct {
			SessionToken string `json:"sessionToken"`
			UserID       int    `json:"userID"`
		}{}
		decoder := json.NewDecoder(r.Body)
		err := decoder.Decode(&userData)
		if err != nil {
			log.Println(err.Error()) //decode fail
			w.WriteHeader(http.StatusBadRequest)
			return
		}

		//verify that the user making the request is allowed
		permissionLevel, err := other.GetPermissionFromToken(userData.SessionToken)
		if err != nil {
			log.Println(err.Error())
			w.WriteHeader(http.StatusInternalServerError)
			return
		}
		if permissionLevel != 0 {
			w.WriteHeader(http.StatusUnauthorized)
			return
		}

		err = other.DeleteUser(userData.UserID)
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
