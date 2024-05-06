package handlers

import (
	"EgressAPI/other"
	"encoding/json"
	"log"
	"net/http"
)

func UserData(w http.ResponseWriter, r *http.Request) {
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
		//create object to send data via json
		data := struct {
			Users []other.Users `json:"Users"`
		}{}

		users, err := other.SeeUsers(token)
		if err != nil {
			log.Println(err.Error())
			return
		}
		data.Users = users
		encoder := json.NewEncoder(w)
		err = encoder.Encode(data)
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
