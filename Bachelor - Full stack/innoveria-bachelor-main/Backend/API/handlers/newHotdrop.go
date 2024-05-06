package handlers

import (
	"API/other"
	"bytes"
	"encoding/json"
	"log"
	"net/http"
)

func NewHotDrop(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Access-Control-Allow-Headers", "Content-Type")
	w.Header().Set("Access-Control-Allow-Origin", ""+other.WebsiteURL)
	w.Header().Set("Access-Control-Allow-Methods", "POST, PUT, DELETE, OPTIONS")
	if r.Method == "POST" {
		decoder := json.NewDecoder(r.Body)
		//creates an anonymous struct to reduce clutter in structs file
		data := struct {
			Name           string `json:"name"`
			Eui            string `json:"eui"`
			Desc           string `json:"desc"`
			MachineNr      string `json:"machine_nr"`
			BuildingName   string `json:"building_name"`
			DepartmentName string `json:"department_name"`
			ExpectedUse    int    `json:"expected_use"`
			Voltage        int    `json:"voltage"`
			SessionToken   string `json:"sessionToken"`
		}{}
		err := decoder.Decode(&data)
		if err != nil {
			println(err.Error())
			w.WriteHeader(http.StatusBadRequest)
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
		UserID, err := other.GetUserIDFromToken(data.SessionToken)
		if err != nil {
			log.Println(err.Error())
			w.WriteHeader(http.StatusBadRequest)
			return
		}
		CompID, err := other.GetCompanyFromUserID(UserID)
		if err != nil {
			log.Println(err.Error())
			w.WriteHeader(http.StatusBadRequest)
			return
		}
		//get the building id
		building, err := other.GetBuildingIdByName(data.BuildingName, CompID)
		if err != nil {
			log.Println(err.Error())
			w.WriteHeader(http.StatusBadRequest)
			return
		}

		department := 0 //check if it is intended to have a department, if not, skip
		if data.DepartmentName != "" && data.DepartmentName != " " {
			department, err = other.GetDepartmentIdByName(data.DepartmentName, CompID)
			if err != nil {
				log.Println(err.Error())
				w.WriteHeader(http.StatusBadRequest)
				return
			}
		}

		//calls function to add hotdrop
		err = addHotdrop(data.Eui, data.Name, data.Desc)
		if err != nil {
			println(err.Error())
			w.WriteHeader(http.StatusInternalServerError)
			return
		}

		//Setup data to send it to db.
		dbMachine := other.Machine{
			EUI:          data.Eui,
			Name:         data.Name,
			ExpectedUse:  data.ExpectedUse,
			MachineNr:    data.MachineNr,
			BuildingID:   building,
			DepartmentID: department,
			Voltage:      data.Voltage,
		}
		err = other.InsertSensor(dbMachine, 0)
		if err != nil {
			log.Println(err.Error())
			w.WriteHeader(http.StatusInternalServerError)
			return
		}

	} else if r.Method == http.MethodPut {
		decoder := json.NewDecoder(r.Body)
		//creates an anonymous struct to reduce clutter in structs file
		data := struct {
			Name           string `json:"name"`
			Eui            string `json:"eui"`
			MachineNr      string `json:"machine_nr"`
			BuildingName   string `json:"building_name"`
			DepartmentName string `json:"department_name"`
			ExpectedUse    int    `json:"expected_use"`
			Voltage        int    `json:"voltage"`
			SessionToken   string `json:"sessionToken"`
		}{}
		err := decoder.Decode(&data)
		if err != nil {
			println(err.Error())
			w.WriteHeader(http.StatusBadRequest)
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
		UserID, err := other.GetUserIDFromToken(data.SessionToken)
		if err != nil {
			log.Println(err.Error())
			w.WriteHeader(http.StatusBadRequest)
			return
		}
		CompID, err := other.GetCompanyFromUserID(UserID)
		if err != nil {
			log.Println(err.Error())
			w.WriteHeader(http.StatusBadRequest)
			return
		}
		//get the building id
		building, err := other.GetBuildingIdByName(data.BuildingName, CompID)
		if err != nil {
			log.Println(err.Error())
			w.WriteHeader(http.StatusBadRequest)
			return
		}
		department := 0 //check if it is intended to have a department, if not, skip
		if data.DepartmentName != "" {
			department, err = other.GetDepartmentIdByName(data.DepartmentName, CompID)
			if err != nil {
				log.Println(err.Error())
				w.WriteHeader(http.StatusBadRequest)
				return
			}
		}
		//Setup data to send it to db.
		dbMachine := other.Machine{
			EUI:          data.Eui,
			Name:         data.Name,
			ExpectedUse:  data.ExpectedUse,
			MachineNr:    data.MachineNr,
			BuildingID:   building,
			DepartmentID: department,
			Voltage:      data.Voltage,
		}

		err = other.EditSensor(dbMachine)
		if err != nil {
			log.Println(err.Error())
			w.WriteHeader(http.StatusInternalServerError)
			return
		}
	} else if r.Method == http.MethodDelete {
		decoder := json.NewDecoder(r.Body)
		//creates an anonymous struct to reduce clutter in structs file
		data := struct {
			Eui          string `json:"eui"`
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

		err = other.DoNewRequest(nil, other.CHIRP_URL+"api/devices/"+data.Eui, http.MethodDelete)
		if err != nil {
			log.Println(err.Error())
			w.WriteHeader(http.StatusInternalServerError)
			return
		}

		err = other.DeleteSensor(data.Eui)
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

// AddHotdrop adds a device to chirpstack using their API, you technically only need eui but name and desc are good too
func addHotdrop(eui string, name string, description string) error {
	device := other.NewHotdropDevice(eui, name, description)

	//The Chirpstack API needs a parent object called "device"
	temp := struct {
		Dev other.OutputHotdrop `json:"device"`
	}{}
	temp.Dev = device //Setting the data for the device (took me too long to spot)

	buf := new(bytes.Buffer) //create a new buffer to store the encoded json
	err := json.NewEncoder(buf).Encode(temp)
	if err != nil {
		return err
	}

	err = other.DoNewRequest(buf, other.CHIRP_URL+"api/devices", http.MethodPost)
	if err != nil {
		return err
	}

	//create the OTAA key and send it to the server (it is just a constant)
	activation := struct {
		Actv struct {
			NwkKey string `json:"nwkKey"`
		} `json:"deviceKeys"`
	}{}
	activation.Actv.NwkKey = "578AEE2D8FC7999FECD3DFFD25F66A0F"
	buf2 := new(bytes.Buffer)

	err = json.NewEncoder(buf2).Encode(activation)
	if err != nil {
		return err
	}
	//do the request
	err = other.DoNewRequest(buf2, other.CHIRP_URL+"api/devices/"+eui+"/keys", http.MethodPost)
	if err != nil {
		return err
	}
	return nil
}

func deleteHotdrop(eui string) {

}
