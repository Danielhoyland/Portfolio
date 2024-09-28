package unisearcher

import (
	"encoding/json"
	"io/ioutil"
	"net/http"
)

func GetApiCountry(apiEndpoint string) ([]Country, error) {
	// Make an HTTP GET request to the API endpoint.
	resp, err := http.Get(apiEndpoint)
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()

	// Read the response body into a byte array.
	data, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		return nil, err
	}

	// Unmarshal the byte array into a slice of maps.
	var countries []Country
	err = json.Unmarshal(data, &countries)
	if err != nil {
		return nil, err
	}

	return countries, nil
}

func GetApiUni(apiEndpoint string) ([]University, error) {
	// Make an HTTP GET request to the API endpoint.
	resp, err := http.Get(apiEndpoint)
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()

	// Read the response body into a byte array.
	uniData, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		return nil, err
	}

	// Unmarshal the university data into a University struct.
	var university []University
	err = json.Unmarshal(uniData, &university)
	if err != nil {
		return nil, err
	}

	return university, nil
}
