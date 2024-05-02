package handlers

import (
	"API/database"
	"encoding/json"
	"net/http"
)
//Handler to make or replace allready existing session token with new one
func MakeSessionTokenHandler(w http.ResponseWriter, r *http.Request) {
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

		authed := database.LoginAuth(user.Email, user.Password)
		if authed {
			//Successful login
			userData, err := database.MakeSessionToken(user.Email)
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

		} else {
			w.WriteHeader(http.StatusUnauthorized)
		}

	} else if r.Method == "OPTIONS" {
		w.WriteHeader(http.StatusOK)
	} else {
		w.WriteHeader(http.StatusTeapot)
	}
}
