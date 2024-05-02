package handlers

import (
	"API/database"
	"encoding/json"
	"fmt"
	"net/http"
	"reflect"
)
//Handler to add a item with POST or to delete an item with DELETE
func AddItem(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Access-Control-Allow-Headers", "Content-Type")
	w.Header().Set("Access-Control-Allow-Origin", "http://10.212.170.10:3000")
	w.Header().Set("Access-Control-Allow-Methods", "POST, OPTIONS, DELETE")
	if r.Method == "POST" {
		data := new(database.UserAddItem)

		decoder := json.NewDecoder(r.Body)
		err := decoder.Decode(&data)
		if err != nil {
			w.WriteHeader(http.StatusBadRequest)
			return
		}
		user := data.TUser
		product := data.TProduct
		userList := data.TUList
		if database.ValidateProduct(product) {
			err = database.InsertUserProduct(user.Token, product.Name, product.BaseMeasure, product.TypeFood, userList.ExpirationDate, userList.Amount, userList.ImageId, userList.Favorite)
			if err != nil {
				w.WriteHeader(http.StatusInternalServerError)
				return
			}
			w.WriteHeader(http.StatusOK)
		} else {
			w.WriteHeader(http.StatusBadRequest)
		}
	} else if r.Method == "DELETE" {
		data := new(database.UserAddItem)
		decoder := json.NewDecoder(r.Body)
		err := decoder.Decode(&data)
		if err != nil {
			w.WriteHeader(http.StatusBadRequest)
			println(err.Error())
			return
		}
		err = database.DeleteUserProduct(data.TUser.Token, data.TProduct.Id)
		if err != nil {
			w.WriteHeader(http.StatusInternalServerError)
			return
		}
		w.WriteHeader(http.StatusOK)

	} else if r.Method == "OPTIONS" {
		w.WriteHeader(http.StatusOK)
	} else {
		w.WriteHeader(http.StatusTeapot)
	}
}
//A handler to set a bool in the database to opposite of what it is with POST
func SetFavorite(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Access-Control-Allow-Headers", "Content-Type")
	w.Header().Set("Access-Control-Allow-Origin", "http://10.212.170.10:3000")
	w.Header().Set("Access-Control-Allow-Methods", "POST, OPTIONS")

	if r.Method == "POST" {
		var jsonData map[string]interface{}
		decoder := json.NewDecoder(r.Body)
		err1 := decoder.Decode(&jsonData)
		if err1 != nil {
			println(err1.Error())
			w.WriteHeader(http.StatusBadRequest)
			return
		}
		if jsonData["token"] == nil || jsonData["id"] == nil {
			w.WriteHeader(http.StatusBadRequest)
			return
		}
		var token = reflect.ValueOf(jsonData["token"]).String()
		var id = int(reflect.ValueOf(jsonData["id"]).Float())
		err := database.InsertFavorite(token, id)
		if err != nil {
			println(err.Error())
			w.WriteHeader(http.StatusBadRequest)
			return
		}

	} else if r.Method == "OPTIONS" {

	} else {
		w.WriteHeader(http.StatusTeapot)
	}
}
//Handler to edit an item with POST
func ChangeAmountHandler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Access-Control-Allow-Headers", "Content-Type")
	w.Header().Set("Access-Control-Allow-Origin", "http://10.212.170.10:3000")
	w.Header().Set("Access-Control-Allow-Methods", "POST, OPTIONS")

	if r.Method == "POST" {
		data := struct {
			SessionId      string  `json:"sessionId"`
			ProductId      int     `json:"productId"`
			Amount         float64 `json:"Amount"`
			ExpirationDate string  `json:"expirationDate"`
		}{
			SessionId:      "",
			ProductId:      -1,
			Amount:         0,
			ExpirationDate: "",
		}
		decoder := json.NewDecoder(r.Body)
		err := decoder.Decode(&data)
		if err != nil {
			fmt.Println(err.Error())
			w.WriteHeader(http.StatusBadRequest)
			return
		}
		email, err := database.GetEmailBySessionToken(data.SessionId)
		if err != nil {
			fmt.Println(err.Error())
			w.WriteHeader(http.StatusNotFound) //could not find the user with that session token
			return
		}
		err = database.InsertUserProductAmount(email, data.ProductId, data.Amount, data.ExpirationDate)
		if err != nil {
			w.WriteHeader(http.StatusInternalServerError)
			return
		}
		w.WriteHeader(http.StatusOK)
		//if success: return 200 OK, if not return 500 internal server error

	} else if r.Method == "OPTIONS" {

	} else {
		w.WriteHeader(http.StatusTeapot)
	}
}
