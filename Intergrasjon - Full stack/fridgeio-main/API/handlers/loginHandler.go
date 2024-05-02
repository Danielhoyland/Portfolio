package handlers

import (
	"API/database"
	"encoding/json"
	"fmt"
	"net/http"
)
//Handler function to send all data to website with POST
func GetData(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Access-Control-Allow-Headers", "Content-Type")
	w.Header().Set("Access-Control-Allow-Origin", "http://10.212.170.10:3000")
	w.Header().Set("Access-Control-Allow-Methods", "POST, OPTIONS")
	if r.Method == "POST" {
		w.Header().Set("content-type", "application/json")
		var user = new(database.User)

		err := json.NewDecoder(r.Body).Decode(&user)
		if err != nil {
			http.Error(w, err.Error(), http.StatusBadRequest)
			return
		}

		userData, err := database.RequestAll(user.Token)
		if err != nil {
			w.WriteHeader(http.StatusInternalServerError)
			return
		}
		_ = json.NewEncoder(w).Encode(userData)
		if err != nil {
			w.WriteHeader(http.StatusInternalServerError)
			return
		}
		w.WriteHeader(http.StatusOK)

	} else if r.Method == "OPTIONS" {
		w.WriteHeader(http.StatusOK)
	} else {
		w.WriteHeader(http.StatusTeapot)
	}
}
//Handler to add a new user with POST or delete a users data with DELETE
func NewUserHandler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Access-Control-Allow-Headers", "Content-Type")
	w.Header().Set("Access-Control-Allow-Origin", "http://10.212.170.10:3000")
	w.Header().Set("Access-Control-Allow-Methods", "POST, OPTIONS, DELETE")
	if r.Method == "POST" {
		var user = new(database.User)

		err := json.NewDecoder(r.Body).Decode(&user)
		if err != nil {
			http.Error(w, err.Error(), http.StatusBadRequest)
			return
		}
		if user.Email != "" && user.Username != "" && user.Password != "" {
			database.NewUser(user.Email, user.Username, user.Password)
		} else {
			w.WriteHeader(http.StatusBadRequest)
		}

	} else if r.Method == "OPTIONS" {
		w.WriteHeader(http.StatusOK)
	} else if r.Method == "DELETE" {
		var user = new(database.User)

		err := json.NewDecoder(r.Body).Decode(&user)
		if err != nil {
			http.Error(w, err.Error(), http.StatusBadRequest)
			return
		}

		err = database.DeleteUser(user.Token)
		if err != nil {
			fmt.Println(err.Error())
			w.WriteHeader(http.StatusInternalServerError)
			return
		}
		w.WriteHeader(http.StatusOK)
	} else {
		w.WriteHeader(http.StatusTeapot)
	}
}
