package handlers

import (
	"EgressAPI/other"
	"encoding/json"
	"golang.org/x/crypto/bcrypt"
	"log"
	"net/http"
	"time"
)

func LoginHandler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Access-Control-Allow-Headers", "Content-Type")
	w.Header().Set("Access-Control-Allow-Origin", ""+other.WebsiteURL)
	w.Header().Set("Access-Control-Allow-Methods", "POST, OPTIONS, GET")

	if r.Method == http.MethodPost { //login
		user := other.Users{}
		decoder := json.NewDecoder(r.Body)
		err := decoder.Decode(&user)
		if err != nil {
			log.Println(err.Error())
			w.WriteHeader(http.StatusBadRequest)
			return
		}
		passHash, err := other.GetHashedPasswordFromEmail(user.Email)
		if err != nil {
			log.Println(err.Error())
			w.WriteHeader(http.StatusUnauthorized)
			return
		}
		pass := []byte(user.Password)
		err = bcrypt.CompareHashAndPassword(passHash, pass)
		if err != nil {
			w.WriteHeader(http.StatusUnauthorized) //failed the comparison, return unathorized
			log.Println(err.Error())
			return
		}
		//Create new session token
		sessionToken, err := other.MakeSessionToken(user.Email)
		if err != nil {
			log.Println(err.Error())
			return
		}

		data := struct {
			SessionToken string          `json:"sessionToken"`
			Profile      []other.Profile `json:"profile"`
		}{}

		profile, err := other.GetProfile(sessionToken)
		if err != nil {
			log.Println(err.Error())
			return
		}

		data.Profile = profile
		data.SessionToken = sessionToken

		encoder := json.NewEncoder(w)

		err = encoder.Encode(data)
		if err != nil {
			w.WriteHeader(http.StatusInternalServerError)
			log.Println(err.Error())
			return
		}
		//currentTime := time.Now()

	} else if r.Method == http.MethodGet { //logout maybe?

	} else if r.Method == http.MethodOptions {

	} else {
		w.WriteHeader(http.StatusMethodNotAllowed)
	}

}

func AdminLogin(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Access-Control-Allow-Headers", "Content-Type")
	w.Header().Set("Access-Control-Allow-Origin", ""+other.ADMIN_WEB_URL)
	w.Header().Set("Access-Control-Allow-Methods", "POST, OPTIONS")
	if r.Method == http.MethodPost { //loginn
		user := other.Users{}
		decoder := json.NewDecoder(r.Body)
		err := decoder.Decode(&user)
		if err != nil {
			log.Println(err.Error())
			w.WriteHeader(http.StatusBadRequest)
			return
		}
		passHash, err := other.GetHashedPasswordFromEmail(user.Email)
		if err != nil {
			log.Println(err.Error())
			w.WriteHeader(http.StatusUnauthorized)
			return
		}
		pass := []byte(user.Password)
		err = bcrypt.CompareHashAndPassword(passHash, pass)
		if err != nil {
			w.WriteHeader(http.StatusUnauthorized) //failed the comparison, return unathorized
			if err != nil {
				log.Println(err.Error())
			}
			return
		}
		//Create new session token
		sessionToken, err := other.MakeSessionToken(user.Email)
		if err != nil {
			log.Println(err.Error())
			return
		}
		perm, err := other.GetPermissionFromToken(sessionToken)
		if err != nil {
			w.WriteHeader(http.StatusInternalServerError)
			log.Println(err.Error())
			return
		}
		if perm != -1 {
			w.WriteHeader(http.StatusUnauthorized)
			log.Println("someone attempted to log into sysadmin using normal account!")
			return
		}

		data := struct {
			SessionToken string          `json:"sessionToken"`
			Profile      []other.Profile `json:"profile"`
		}{}

		profile, err := other.GetProfile(sessionToken)
		if err != nil {
			log.Println(err.Error())
			return
		}

		data.Profile = profile
		data.SessionToken = sessionToken

		encoder := json.NewEncoder(w)

		err = encoder.Encode(data)
		if err != nil {
			w.WriteHeader(http.StatusInternalServerError)
			log.Println(err.Error())
			return
		}
		//currentTime := time.Now()

	} else if r.Method == http.MethodGet { //logout maybe?

	} else if r.Method == http.MethodOptions {

	} else {
		w.WriteHeader(http.StatusMethodNotAllowed)
	}
}

// ClearSession clears all session tokens that are no longer valid, it should be run periodically, such as once a day
func ClearSession(period time.Duration) {
	t := time.NewTicker(period * time.Hour)
	defer t.Stop()
	for {
		select {
		case <-t.C:
			other.ClearSessionTokens()
		}
	}
}
