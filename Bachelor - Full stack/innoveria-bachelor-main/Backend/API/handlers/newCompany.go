package handlers

import (
	"API/other"
	"encoding/json"
	"log"
	"net/http"
)

func NewCompany(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Access-Control-Allow-Headers", "Content-Type")
	w.Header().Set("Access-Control-Allow-Origin", ""+other.ADMIN_WEB_URL)
	w.Header().Set("Access-Control-Allow-Methods", "POST, OPTIONS")
	if r.Method == http.MethodPost {
		data := struct {
			SessionToken string `json:"sessionToken"`
			CompanyName  string `json:"companyName"`
			Email        string `json:"email"`
			Password     string `json:"password"`
		}{}
		decoder := json.NewDecoder(r.Body)
		err := decoder.Decode(&data)
		if err != nil {
			log.Println(err.Error())
			w.WriteHeader(http.StatusBadRequest)
			return
		}
		perm, err := other.GetPermissionFromToken(data.SessionToken)
		if err != nil {
			log.Println(err.Error())
			w.WriteHeader(http.StatusUnauthorized)
			return
		}
		//check if they are allowed to do request
		if perm != -1 {
			w.WriteHeader(http.StatusUnauthorized)
			return
		}
		if data.CompanyName == "" || data.Email == "" || data.Password == "" {
			w.WriteHeader(http.StatusBadRequest)
			return
		}
		err = other.InsertCompany(data.CompanyName, data.Email, data.Password)
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
