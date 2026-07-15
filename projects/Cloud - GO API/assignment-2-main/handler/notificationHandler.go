package handler

import (
	"assignment-2/helper"
	"bytes"
	"crypto/hmac"
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"fmt"
	"io"
	"log"
	"math/rand"
	"net/http"
	"strconv"
	"strings"
	"time"
)

var Webhooks []helper.WebhookRegistration
var Secret []byte
var SignatureKey = "X-SIGNATURE"

/*
get request: /energy/v1/notifications/({id}(optional))
post request: /energy/v1/notifications/
delete request: /energy/v1/notifications/{id}
*/

// NotificationHandler handles requests for webhooks
func NotificationHandler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("content-type", "application/json")
	switch r.Method {
	case http.MethodGet: //if the request is a GET request
		fmt.Println("Handling GET request...")
		NotifGetRequest(w, r)
	case http.MethodPost:
		fmt.Println("Handling POST request...")
		NotifPostRequest(w, r)
	case http.MethodDelete:
		fmt.Println("Handling DELETE request...")
		NotifDeleteRequest(w, r)
	default: //otherwise give error
		fmt.Fprintln(w, "Sorry this method is not implemented yet")
	}
}

// NotifGetRequest handles GET requests for webhooks
func NotifGetRequest(w http.ResponseWriter, r *http.Request) {
	// trims the last "/" off before splitting
	tags := strings.Split(strings.Trim(r.URL.Path, "/"), "/")
	// if there is a tag added
	if len(tags) > 3 {
		tag := tags[3]
		// goes through webhooks and prints the ones with the same ID
		for _, val := range Webhooks {
			if val.WebhookID == tag {
				err := json.NewEncoder(w).Encode(val)
				if err != nil {
					http.Error(w, "Something went wrong: "+err.Error(), http.StatusInternalServerError)
				}
				break // exits loop when found webhook
			}
		}
		// if there is no extra tag
	} else {
		// prints webhooks if there are any
		if len(Webhooks) > 0 {
			err := json.NewEncoder(w).Encode(Webhooks)
			if err != nil {
				http.Error(w, "Something went wrong: "+err.Error(), http.StatusInternalServerError)
			}
		} else {
			fmt.Fprintln(w, "No webhooks")
		}
	}
}

// notifPostRequest handles POST requests for webhooks
func NotifPostRequest(w http.ResponseWriter, r *http.Request) {
	// creating webhook variable
	id := GenID()
	webhook := helper.WebhookRegistration{WebhookID: id, Calls: 0}
	err := json.NewDecoder(r.Body).Decode(&webhook)
	if err != nil {
		http.Error(w, "Something went wrong: "+err.Error(), http.StatusBadRequest)
		return
	}
	// Adding webhook to webhooks
	Webhooks = append(Webhooks, webhook)
	log.Println("Webhook " + webhook.Url + " has been registered.")
	// Print index of recorded webhook as response - note: in practice you would return some unique identifier, not exposing DB internals
	fmt.Println("Number of webhooks: ", len(Webhooks))
	// Send response
	json.NewEncoder(w).Encode(id)
}

// notifDeleteRequest handles DELETE requests for webhooks
func NotifDeleteRequest(w http.ResponseWriter, r *http.Request) {
	tags := strings.Split(strings.Trim(r.URL.Path, "/"), "/")
	// only deletes if the ID is passed
	if len(tags) > 3 {
		id := tags[3]
		for i := range Webhooks {
			if Webhooks[i].WebhookID == id {
				// creates new slice from all webhooks before and after "i"
				Webhooks = append(Webhooks[:i], Webhooks[i+1:]...)
				break
			}
		}
	}
}

// NotifCountryCall calls url of the webhook corresponding to given iso
func NotifCountryCall(iso string, method string) {
	for i := range Webhooks {
		wIso := Webhooks[i].Country
		if wIso == iso {
			url := Webhooks[i].Url
			Webhooks[i].Calls++
			fmt.Println("Updating webhook: ", iso)
			log.Println("Attempting invocation of url " + url)
			content := "The country: " + wIso + " got searched for"

			req, err := http.NewRequest(method, url, bytes.NewReader([]byte(content)))

			/// BEGIN: HEADER GENERATION FOR CONTENT-BASED VALIDATION

			// Hash content (for content-based validation; not relevant for URL-based validation)
			mac := hmac.New(sha256.New, Secret)
			_, err = mac.Write([]byte(content))
			if err != nil {
				log.Printf("%v", "Error during content hashing. Error:", err)
				return
			}
			// Convert hash to string & add to header to transport to client for validation
			req.Header.Add(SignatureKey, hex.EncodeToString(mac.Sum(nil)))

			/// END: CONTENT-BASED VALIDATION

			// Perform invocation
			client := http.Client{}
			res, err := client.Do(req)
			if err != nil {
				log.Println("Error in HTTP request. Error:", err)
				return
			}

			// Read the response
			response, err := io.ReadAll(res.Body)
			if err != nil {
				log.Println("Something is wrong with invocation response. Error:", err)
				return
			}

			log.Println("Webhook " + url + " invoked. Received status code " + strconv.Itoa(res.StatusCode) +
				" and body: " + string(response))

		}
	}
}

// genID generates an ID for webhooks and checks for duplicates
func GenID() string {
	const chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
	for {
		id := make([]byte, 8)
		rand.Seed(time.Now().UnixNano()) // seed the random number generator
		for i := range id {
			id[i] = chars[rand.Intn(len(chars))]
		}
		// check if the ID already exists
		found := false
		for _, hook := range Webhooks {
			if hook.WebhookID == string(id) {
				found = true
				break
			}
		}
		if !found {
			return string(id)
		}
	}
}
