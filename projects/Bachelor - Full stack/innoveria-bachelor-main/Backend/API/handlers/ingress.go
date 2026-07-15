package handlers

import (
	"API/other"
	"encoding/json"
	"fmt"
	"log"
	"net/http"
	"time"
)

func HotDropData(w http.ResponseWriter, r *http.Request) {
	decoder := json.NewDecoder(r.Body)
	object := struct {
		Timestamp string `json:"time"`
		Device    struct {
			Eui string `json:"devEui"`
		} `json:"deviceInfo"`
		Data struct {
			Temp         float64 `json:"temperatureCelsius"`
			AvgAmps      float64 `json:"averageAmps"`
			MaxAmps      float64 `json:"maximumAmps"`
			Voltage      float64 `json:"capacitorVoltage"`
			Accumulation float64 `json:"ampHourAccumulation"`
			MinAmps      float64 `json:"minimumAmps"`
		} `json:"object"`
	}{}
	err := decoder.Decode(&object)
	if err != nil {
		fmt.Println("error reading the json, error: " + err.Error())
		w.WriteHeader(http.StatusBadRequest)
		return
	}

	//time location, needs to be GMT-1 instead of +1 for some reason
	location, err := time.LoadLocation("Etc/GMT-1")
	if err != nil {
		log.Println(err.Error())
	}
	//timestamp, initially set to global time
	date, err := time.Parse(time.RFC3339Nano, object.Timestamp)
	if err != nil {
		println(err.Error())
		w.WriteHeader(http.StatusInternalServerError)
		return
	}
	//changed from global time to norwegian time.
	date = date.In(location)

	//Data to be sent to the database
	sData := other.SensorData{
		EUI:          object.Device.Eui,
		Accumulation: object.Data.Accumulation,
		AVGCurrent:   object.Data.AvgAmps,
		OffsetMin:    object.Data.MinAmps,
		OffsetMax:    object.Data.MaxAmps,
		Voltage:      object.Data.Voltage,
		Temperature:  object.Data.Temp,
		DateTime:     date,
	}
	//send sensorData to the database
	err = other.InsertSensorData(sData)
	if err != nil {
		log.Println(err.Error())
		w.WriteHeader(http.StatusInternalServerError)
		return
	}
}
