package handlers

import (
	"API/other"
	"bytes"
	"encoding/json"
	"log"
	"net/http"
)

func AddGateway(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Access-Control-Allow-Headers", "Content-Type")
	w.Header().Set("Access-Control-Allow-Origin", ""+other.WebsiteURL)
	w.Header().Set("Access-Control-Allow-Methods", "POST, DELETE, PUT, OPTIONS")
	if r.Method == http.MethodPost {
		decoder := json.NewDecoder(r.Body)
		//creates an anonymous struct to reduce clutter in structs file
		data := struct {
			Name  string `json:"name"`
			Eui   string `json:"eui"`
			Token string `json:"sessionToken"`
		}{}
		err := decoder.Decode(&data)
		if err != nil {
			println(err.Error())
			return
		}

		valid, err := other.IsTokenStillActive(data.Token)
		if !valid {
			w.WriteHeader(http.StatusUnauthorized)
			log.Println("Session token expired or non-existing")
			if err != nil {
				log.Println(err.Error())
			}
			return
		}

		//calls function to add gateway
		err = addGateway(data.Eui, data.Name)
		if err != nil {
			log.Println(err.Error())
			w.WriteHeader(http.StatusInternalServerError)
			return
		}
		err = other.AddGateway(data.Token, data.Eui, data.Name)
		if err != nil {
			log.Println(err.Error())
			w.WriteHeader(http.StatusInternalServerError)
			return
		}
	} else if r.Method == http.MethodPut {
		decoder := json.NewDecoder(r.Body)
		//creates an anonymous struct to reduce clutter in structs file
		data := struct {
			Name    string `json:"name"`
			OldName string `json:"oldName"`
			Eui     string `json:"eui"`
			OldEui  string `json:"oldEui"`
			Token   string `json:"sessionToken"`
		}{}
		err := decoder.Decode(&data)
		if err != nil {
			println(err.Error())
			return
		}
		valid, err := other.IsTokenStillActive(data.Token)
		if !valid {
			log.Println("Session token expired or not valid!")
			w.WriteHeader(http.StatusUnauthorized)
			if err != nil {
				log.Println(err.Error())
			}
			return
		}
		//verify that the user making the request is allowed
		permissionLevel, err := other.GetPermissionFromToken(data.Token)
		if err != nil {
			log.Println(err.Error())
			w.WriteHeader(http.StatusInternalServerError)
			return
		}
		if permissionLevel != 0 {
			w.WriteHeader(http.StatusUnauthorized)
			return
		}
		UserID, err := other.GetUserIDFromToken(data.Token)
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
		//calls function to edit building
		err = other.EditGateway(data.Eui, data.OldEui, data.Name, data.OldName, CompID)
		if err != nil {
			println(err.Error())
			w.WriteHeader(http.StatusInternalServerError)
			return
		}
	} else if r.Method == http.MethodDelete {
		decoder := json.NewDecoder(r.Body)
		//creates an anonymous struct to reduce clutter in structs file
		data := struct {
			Eui   string `json:"eui"`
			Token string `json:"sessionToken"`
		}{}
		err := decoder.Decode(&data)
		if err != nil {
			println(err.Error())
			return
		}

		valid, err := other.IsTokenStillActive(data.Token)
		if !valid {
			w.WriteHeader(http.StatusUnauthorized)
			log.Println("Session token expired or non-existing")
			if err != nil {
				log.Println(err.Error())
			}
			return
		}
		err = other.DoNewRequest(nil, other.CHIRP_URL+"api/gateways/"+data.Eui, http.MethodDelete)
		if err != nil {
			log.Println(err.Error())
			w.WriteHeader(http.StatusInternalServerError)
			return
		}
		err = other.DeleteGateway(data.Eui)
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

func addGateway(eui string, name string) error {
	device := other.NewGatewayDevice(eui, name)

	//The Chirpstack API needs a parent object called "device"
	temp := struct {
		Dev other.OutputGateway `json:"gateway"`
	}{}
	temp.Dev = device //Setting the data for the device (took me too long to spot)

	buf := new(bytes.Buffer) //create a new buffer to store the encoded json
	err := json.NewEncoder(buf).Encode(temp)
	if err != nil {
		return err
	}

	err = other.DoNewRequest(buf, other.CHIRP_URL+"api/gateways", http.MethodPost)
	if err != nil {
		return err
	}
	return nil
}
