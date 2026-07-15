package database

import (
	"golang.org/x/crypto/bcrypt"
)

func LoginAuth(email string, pass string) bool {

	user := UserDataRequest(email)

	err := bcrypt.CompareHashAndPassword([]byte(user.Password), []byte(pass))
	if err == nil {
		return true
	}
	return false
}

func ValidateProduct(product Product) bool {
	if product.Name != "" && product.BaseMeasure != "" && product.TypeFood != "" {
		return true
	}
	return false
}

func ValidateUserLists(list UsersLists) bool {
	if list.Amount != 0 {
		return true
	}
	return false
}

func ValidateRecipe(recipe Recipe) bool {
	if recipe.Name != "" && recipe.Description != "" && recipe.Portion > 0 {
		return true
	}
	return false
}
