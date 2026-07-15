package handlers

import (
	"EgressAPI/other"
	"encoding/json"
	"log"
	"net/http"
)

func EnoekData(w http.ResponseWriter, r *http.Request) {
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

		// Create object to send data via JSON
		data := struct {
			Enoek []other.EnokSuggestionMeasures2 `json:"enoek"`
		}{}

		enoeks, err := other.GetAllEnoek(token)
		if err != nil {
			log.Println(err.Error())
			w.WriteHeader(http.StatusInternalServerError)
			return
		}

		// Populate data with enoeks
		for i := 0; i < len(enoeks); i++ {

			process, err := other.GetProcessNameById(enoeks[i].ProcessID)
			if err != nil {
				log.Println(err.Error())
				w.WriteHeader(http.StatusBadRequest)
				return
			}
			person, err := other.GetPerson(enoeks[i].Author)
			if err != nil {
				log.Println(err.Error())
				w.WriteHeader(http.StatusBadRequest)
				return
			}

			data.Enoek = append(data.Enoek, other.EnokSuggestionMeasures2{
				ID:          enoeks[i].ID,
				Header:      enoeks[i].Header,
				Description: enoeks[i].Description,
				Author:      person,
				StartDate:   enoeks[i].StartDate,
				StopDate:    enoeks[i].StopDate,
				Active:      enoeks[i].Active,
				Process:     process,
				Approved:    enoeks[i].Approved,
			})
		}

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
