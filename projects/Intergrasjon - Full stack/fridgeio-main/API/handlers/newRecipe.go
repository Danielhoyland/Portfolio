package handlers

import (
	"API/database"
	"encoding/json"
	"net/http"
)
//add an recipe to database with POST or delete an recipe with DELETE
func AddRecipe(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Access-Control-Allow-Headers", "Content-Type")
	w.Header().Set("Access-Control-Allow-Origin", "http://10.212.170.10:3000")
	w.Header().Set("Access-Control-Allow-Methods", "POST, OPTIONS, DELETE")
	if r.Method == "POST" {
		data := new(database.UserAddRecipe)

		decoder := json.NewDecoder(r.Body)
		err := decoder.Decode(&data)
		if err != nil {
			w.WriteHeader(http.StatusBadRequest)
			return
		}
		user := data.TUser
		recipe := data.TRecipe
		products := data.TRequiredProducts
		amounts := data.TAmount
		if database.ValidateRecipe(recipe) {
			err = database.InsertUserRecipe(user.Token, recipe.Name, recipe.Description, recipe.Portion, products, amounts)
			if err != nil {
				w.WriteHeader(http.StatusInternalServerError)
				return
			}
			w.WriteHeader(http.StatusOK)
		} else {
			w.WriteHeader(http.StatusBadRequest)
		}
	} else if r.Method == "DELETE" {
		data := new(database.UserDeleteRecipe)
		decoder := json.NewDecoder(r.Body)
		err := decoder.Decode(&data)
		if err != nil {
			w.WriteHeader(http.StatusBadRequest)
			println(err.Error())
			return
		}
		err = database.DeleteUserRecipe(data.TUser.Token, data.TRecipe.Id)
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
//Get just the recipe data wiht POST
func GetRecipes(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Access-Control-Allow-Headers", "Content-Type")
	w.Header().Set("Access-Control-Allow-Origin", "http://10.212.170.10:3000")
	w.Header().Set("Access-Control-Allow-Methods", "POST")

	if r.Method == "POST" {
		data := new(database.User)

		decoder := json.NewDecoder(r.Body)
		err := decoder.Decode(&data)
		if err != nil {
			w.WriteHeader(http.StatusBadRequest)
			return
		}
		user := data.Token
		recipeAll, _, err := database.UserRecipes(user)
		if err != nil {
			w.WriteHeader(http.StatusInternalServerError)
			return
		}
		err = json.NewEncoder(w).Encode(recipeAll)
		if err != nil {
			w.WriteHeader(http.StatusInternalServerError)
			return
		}
		w.WriteHeader(http.StatusOK)
	} else {
		w.WriteHeader(http.StatusTeapot)
	}
}
//Handler to edit recipe with POST
func EditRecipe(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Access-Control-Allow-Headers", "Content-Type")
	w.Header().Set("Access-Control-Allow-Origin", "http://10.212.170.10:3000")
	w.Header().Set("Access-Control-Allow-Methods", "POST")

	if r.Method == "POST" {
		data := new(database.UserAddRecipe)

		decoder := json.NewDecoder(r.Body)
		err := decoder.Decode(&data)
		if err != nil {
			w.WriteHeader(http.StatusBadRequest)
			return
		}
		user := data.TUser
		recipe := data.TRecipe
		products := data.TRequiredProducts
		amounts := data.TAmount
		if database.ValidateRecipe(recipe) {
			err = database.EditRecipe(user.Token, recipe.Id, recipe.Name, recipe.Description, recipe.Portion, products, amounts)
			if err != nil {
				w.WriteHeader(http.StatusInternalServerError)
				return
			}
			w.WriteHeader(http.StatusOK)
		} else {
			w.WriteHeader(http.StatusBadRequest)
		}
	}
}
