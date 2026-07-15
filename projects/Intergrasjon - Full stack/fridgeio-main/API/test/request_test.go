package test

import (
	"API/database"
	_ "database/sql"
	"fmt"
	_ "github.com/mattn/go-sqlite3"
	"io/ioutil"
	"log"
	"reflect"
	"testing"
)

func TestGetUserData(t *testing.T) {
	database.OpenTestDB()
	defer database.CloseDB()
	email := "odinaa@stud.ntnu.no"

	var user = database.UserDataRequest(email)
	println(user.Email)

	if user.Email != "odinaa@stud.ntnu.no" {
		t.Fatal("wrong result")
	}
}

func TestLoginAuth(t *testing.T) {
	database.OpenTestDB()
	defer database.CloseDB()

	var result = database.LoginAuth("odinaa@stud.ntnu.no", "Fart")

	if !result {
		t.Fatal("wrong result from database")
	}
}

func TestInsertUser(t *testing.T) {
	database.OpenTestDB()
	defer database.CloseDB()

	database.NewUser("example@example.com", "example", "Password")

	var result = database.LoginAuth("example@example.com", "Password")

	if !result {
		t.Fatal("wrong result from database")
	}
}

func setupTestDatabase() {
	sqlDump, err := ioutil.ReadFile("test.txt")
	if err != nil {
		log.Fatal(err)
	}

	statements := string(sqlDump)
	database.ExecSQL(statements)
}
func TestGetUserProductList(t *testing.T) {
	database.OpenTestDB()
	defer database.CloseDB()
	email := "odinaa@stud.ntnu.no"

	plist, ulist, err := database.UserProductList(email)
	if err != nil {
		log.Fatal(err)
	}
	for i, product := range plist {
		fmt.Printf("Name: %s, Amount: %f, Base Measure: %s, Type Food: %s, Expiration Date: %s, Image ID: %d\n",
			product.Name, ulist[i].Amount, product.BaseMeasure, product.TypeFood, ulist[i].ExpirationDate, ulist[i].ImageId)
	}
}
func TestGetUserRecipeList(t *testing.T) {
	database.OpenTestDB()
	defer database.CloseDB()
	email := "odinaa@stud.ntnu.no"

	recipeAll, Recipe, err := database.UserRecipes(email)
	if err != nil {
		log.Fatal(err)
	}
	if recipeAll == nil {
		log.Fatal("Expected non-nil results, but got nil")
	}
	if Recipe == nil {
		log.Fatal("Expected non-nil results, but got nil")
	}
	for _, recipeComponent := range recipeAll {
		// Print RecipeName and Description
		fmt.Printf("RecipeName: %s\nDescription: %s\n Portions: %d\n", recipeComponent.Name, recipeComponent.Description, recipeComponent.Portion)

		// Print PRODUCTS:
		fmt.Println("PRODUCTS:")

		// Iterate through products and print each product with base measure, type, and amount
		for i, product := range recipeComponent.Products {
			fmt.Printf("Product %d:\n", i+1)
			fmt.Printf("Name: %s\nBase Measure: %s\nType: %s\nAmount: %.2f\n\n", product.Name, product.BaseMeasure, product.TypeFood, recipeComponent.Amounts[i])
		}
	}
}

func TestInsertUserProduct(t *testing.T) {
	database.OpenTestDB()
	defer database.CloseDB()

	email := "Daniehoy@stud.ntnu.no"
	productName := "New Product_6"
	baseMeasure := "Grams"
	typeFood := "Dry Goods"
	expirationDate := "2023-09-16"
	amount := 0.0
	imageID := 0

	err := database.InsertUserProduct(email, productName, baseMeasure, typeFood, expirationDate, amount, imageID, false)
	if err != nil {
		t.Fatalf("Error inserting user product: %v", err)
	}

	// Fetch the inserted product
	productList, UsersList, err := database.UserProductList(email)
	if err != nil {
		t.Fatalf("Error fetching user product: %v", err)
	}

	// Check if the inserted product is in the list
	found := false
	for i, product := range productList {
		expectedDate := "2023-09-16" // Expected date in the desired format

		// Format the date retrieved from the database as a string in the desired format
		formattedDate := UsersList[i].ExpirationDate

		if product.Name == productName &&
			product.BaseMeasure == baseMeasure &&
			product.TypeFood == typeFood &&
			formattedDate == expectedDate && // Compare with the expected date format
			UsersList[i].ImageId == imageID { // Compare the amount
			found = true
			break
		}
	}

	if !found {
		t.Fatal("Inserted product not found in user's product list")
	}
}

func TestInsertUserRecipe(t *testing.T) {
	database.OpenTestDB()
	defer database.CloseDB()

	token := "VtSf3fjEzJeKAY_Zk3J_zcHNFftn6-lEquMU2ocoLDM="
	recipeName := "New Recipe3"
	description := "Delicious new recipe4"
	requiredProducts := []int{101, 102, 103}
	amount := []float64{1.5, 300, 2}
	portion := 4

	err := database.InsertUserRecipe(token, recipeName, description, portion, requiredProducts, amount)
	if err != nil {
		t.Fatalf("Error inserting user recipe: %v", err)
	}

	// Fetch the inserted recipe, including the associated products
	RecipeAll, _, err := database.UserRecipes(token)
	if err != nil {
		t.Fatalf("Error fetching user recipes: %v", err)
	}

	// Check if the inserted recipe is in the list
	found := false
	for _, recipe := range RecipeAll {
		if recipe.Name == recipeName && recipe.Description == description {
			found = true
			// Check if the required products are inserted into the recipe
			for _, productName := range requiredProducts {
				foundProduct := false

				for _, product := range recipe.Products {
					if product == product {
						foundProduct = true
						break
					}
				}
				if !foundProduct {
					t.Fatalf("Required product %d not found in the recipe", productName)
				}
			}
			break
		}
	}
	if !found {
		t.Fatal("Inserted recipe not found in user's recipe list")
	}
}

func TestGetAll(t *testing.T) {
	database.OpenTestDB()
	defer database.CloseDB()

	//date2, _ := time.Parse("2006-01-02", "2023-09-07")
	//recipeList := []database.Product{{Id: 0, Name: "Flour", BaseMeasure: "", TypeFood: ""}, {Id: 0, Name: "Eggs", BaseMeasure: "", TypeFood: ""}}
	user := database.User{Username: "Ofin", Email: "odinaa@stud.ntnu.no", Password: "", Token: ""}
	products := []database.Product{{Id: 103, Name: "Eggs", BaseMeasure: "Dozen", TypeFood: "Animal Products"}}          //{Id: 101, Name: "Milk", BaseMeasure: "Liter", TypeFood: "Dairy"}
	userlists := []database.UsersLists{{UserId: "", ProductId: 0, ExpirationDate: "2023-09-06", Amount: 2, ImageId: 0}} //, {UserId: "", ProductId: 0, ExpirationDate: date2, Amount: 1.5, ImageId: 0}
	usersrecipe := []database.Recipe{{Id: 3, Name: "Vegetable Stir-Fry", Description: "Stir-fried vegetables with soy sauce and tofu."}}
	recipeAll := []database.RecipeWithProducts{}

	// Create a new RecipeWithProducts instance
	recipe := database.RecipeWithProducts{
		Recipe: database.Recipe{
			Id:          3,                                                // Replace with the actual recipe ID
			Name:        "Vegetable Stir-Fry",                             // Replace with the actual recipe name
			Description: "Stir-fried vegetables with soy sauce and tofu.", // Replace with the actual description
		},
		Products: []database.Product{
			{
				Id:          103,              // Replace with the actual product ID
				Name:        "Egg",            // Replace with the actual product name
				BaseMeasure: "Dozen",          // Replace with the actual base measure
				TypeFood:    "Animal Product", // Replace with the actual type of food
			},
			{
				Id:          102,         // Replace with the actual product ID
				Name:        "Flour",     // Replace with the actual product name
				BaseMeasure: "Grams",     // Replace with the actual base measure
				TypeFood:    "Dry Goods", // Replace with the actual type of food
			},
		},
		Amounts: []float64{
			2.0,
			500.0,
		},
	}

	// Append the created RecipeWithProducts to the recipeAll slice
	recipeAll = append(recipeAll, recipe)

	userData := database.UserData{UserD: user, Products: products, UserLists: userlists, Recipes: usersrecipe, RecipeInfos: recipeAll}
	result, _ := database.RequestAll("odinaa@stud.ntnu.no")

	if !reflect.DeepEqual(userData, result) {
		t.Fatal("TEST FAILED, RESULT NOT EQUAL")
	}
}

func TestDeleteUserProduct(t *testing.T) {
	database.OpenTestDB()
	defer database.CloseDB()

	email := "odinaa@stud.ntnu.no"
	productName := "Milk" // Replace with the actual product name you want to delete
	productID := 101
	// First, add the product to the user's product list (assuming you have an insertion function)
	if err := database.InsertUserProduct(email, productName, "Liter", "Dairy", "2023-09-15", 1.0, 0, false); err != nil {
		t.Fatalf("Error inserting user product: %v", err)
	}

	// Then, try to delete the product
	err := database.DeleteUserProduct(email, productID)
	if err != nil {
		t.Fatalf("Error deleting user product: %v", err)
	}

	// Verify that the product was successfully deleted
	// You can use another function like UserProductList to check if the product still exists in the user's product list
	productList, usersListInfo, err := database.UserProductList(email)
	if err != nil {
		t.Fatalf("Error fetching user product list: %v", err)
	}

	// Check if the product with the specific name was deleted
	found := false
	for i, product := range productList {
		if product.Name == productName {
			found = true

			// Check if the related users list item was also deleted
			if i < len(usersListInfo) {
				t.Fatalf("Related user's list item was not deleted")
			}
		}
	}

	if found {
		t.Fatalf("Product with name %s still exists in the user's product list", productName)
	}
}

func TestGetSessionToken(t *testing.T) {
	database.OpenTestDB()
	defer database.CloseDB()
	sessionToken, err := database.GetSessionToken("odinaa@stud.ntnu.no")

	// Ensure an error occurred as expected.
	if err != nil {
		t.Errorf("Got error")
	} else if sessionToken != "2" {
		// Ensure no error occurred.
		t.Errorf("Wrong value")
	}
}

func TestInsertSessionToken(t *testing.T) {
	database.OpenTestDB()
	defer database.CloseDB()
	Token, err1 := database.MakeSessionToken("odinaa@stud.ntnu.no")
	sessionToken, err2 := database.GetSessionToken("odinaa@stud.ntnu.no")
	println(Token)
	println(sessionToken)
	// Ensure an error occurred as expected.
	if err1 != nil {
		t.Errorf("Got error1")
	} else if err2 != nil {
		t.Errorf("Got error2")
	} else if sessionToken != Token {
		// Ensure no error occurred.
		t.Errorf("Wrong value")
	}
}

func TestInsertFavorite(t *testing.T) {
	database.OpenTestDB()
	defer database.CloseDB()

	// Insert a user with a session token
	userEmail := "Daniehoy@stud.ntnu.no"
	sessionToken := "1"
	productID := 111

	// Set the product as a favorite
	err := database.InsertFavorite(sessionToken, productID)
	if err != nil {
		t.Fatalf("Error inserting favorite: %v", err)
	}

	// Fetch the product's favorite status
	var isFavorite bool
	err = database.DB.QueryRow("SELECT Favorite FROM USERS_LISTS WHERE User_ID = ? AND Product_ID = ?", userEmail, productID).Scan(&isFavorite)
	if err != nil {
		t.Fatalf("Error fetching favorite status: %v", err)
	}

	if !isFavorite {
		t.Fatal("Product should be set as a favorite, but it's not.")
	}
}

func TestEditRecipe(t *testing.T) {
	// Open and prepare a test database
	database.OpenTestDB()
	defer database.CloseDB()

	// Mock data for the test
	sessionToken := "1"
	recipeID := 1
	recipeName := "SPagett"
	description := "Updated Description"
	portion := 4
	requiredProducts := []int{1, 2, 3}
	amounts := []float64{1.5, 2.0, 3.0}

	// Call the function being tested
	err := database.EditRecipe(sessionToken, recipeID, recipeName, description, portion, requiredProducts, amounts)
	if err != nil {
		t.Fatalf("Error editing recipe: %v", err)
	}
}
