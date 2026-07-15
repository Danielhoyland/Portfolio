package database

import (
	"crypto/rand"
	"database/sql"
	"encoding/base64"
	"errors"
	"fmt"
	"golang.org/x/crypto/bcrypt"
	"io"
	"log"
	"strings"
)

var DB *sql.DB

func RequestAll(sessionToken string) (UserData, error) {
	email, err := GetEmailBySessionToken(sessionToken)
	if err != nil {
		println(err.Error())
		return UserData{}, err
	}

	var data UserData
	user := UserDataRequest(email)
	products, lists, err := UserProductList(email)
	if err != nil {
		println(err.Error())
	}
	userRecipesAll, recipes, err := UserRecipes(email)
	if err != nil {
		println(err.Error())
	}

	user.Password = ""
	data.UserD = user
	data.UserLists = lists
	data.Products = products
	data.Recipes = recipes
	data.RecipeInfos = userRecipesAll
	return data, nil
}

func UserDataRequest(email string) User {

	rows, err := DB.Query("select * from USERS WHERE email = ?", email)
	if err != nil {

		panic(err)
	}

	var user User

	for rows.Next() {
		err := rows.Scan(&user.Email, &user.Username, &user.Password, &user.Token)
		if err != nil {
			log.Fatal(err)
		}
		//log.Println(user.Email, user.Username, user.Password)
	}
	rows.Close()
	finalUser := User{
		Email:    user.Email,
		Username: user.Username,
		Password: user.Password,
	}

	return finalUser
}

func NewUser(email string, username string, pass string) {
	hashed, err := bcrypt.GenerateFromPassword([]byte(pass), 8)
	if err != nil {
		fmt.Println("Error hashing password... aborting insert")
		return
	}
	pass = string(hashed)

	_, err = DB.Exec("INSERT INTO users (Email, Username, Password) VALUES (?,?,?);", email, username, pass)

	if err != nil {
		fmt.Println("ERROR INSERTING DATA!")
		fmt.Println(err)
	}
}

func DeleteUser(sessionToken string) error {
	email, err := GetEmailBySessionToken(sessionToken)
	if err != nil {
		return err
	}

	var hasLists bool
	err = DB.QueryRow("SELECT EXISTS(SELECT 1 FROM users_lists WHERE User_ID = ? LIMIT 1)", email).Scan(&hasLists)
	if err != nil {
		return err
	}

	// Check if there are any records in users_recipe
	var hasRecipes bool
	err = DB.QueryRow("SELECT EXISTS(SELECT 1 FROM users_recipe WHERE User_ID = ? LIMIT 1)", email).Scan(&hasRecipes)
	if err != nil {
		return err
	}
	if hasLists {
		// Delete data from users_list
		_, err = DB.Exec("DELETE FROM users_lists WHERE User_ID = ?", email)
		if err != nil {
			return err
		}
	}
	if hasRecipes {
		// Delete data from users_recipe
		_, err = DB.Exec("DELETE FROM users_recipe WHERE User_ID = ?", email)
		if err != nil {
			return err
		}
	}

	// Delete user from users table
	_, err = DB.Exec("DELETE FROM users WHERE Email = ?", email)
	if err != nil {
		return err
	}

	return nil
}

func UserProductList(email string) ([]Product, []UsersLists, error) {

	var productsInfo []Product
	var usersListInfo []UsersLists

	query := `
        SELECT p.ID, p.Name, p.Base_measure, p.Type_Food, ul.Product_ID ,ul.Expiration_Date, ul.Amount, ul.Image_Id, ul.Favorite
        FROM users_lists ul
        JOIN products p ON ul.Product_Id = p.Id
        WHERE ul.User_Id = ?`

	rows, err := DB.Query(query, email)
	if err != nil {
		return nil, nil, err
	}
	defer rows.Close()

	for rows.Next() {
		var productInfo Product
		var userListInfo UsersLists
		if err := rows.Scan(
			&productInfo.Id,
			&productInfo.Name,
			&productInfo.BaseMeasure,
			&productInfo.TypeFood,
			&userListInfo.ProductId,
			&userListInfo.ExpirationDate, // Update the type to time.Time //updated again back to string
			&userListInfo.Amount,
			&userListInfo.ImageId,
			&userListInfo.Favorite,
		); err != nil {
			return nil, nil, err
		}
		userListInfo.ExpirationDate = strings.Trim(userListInfo.ExpirationDate, "T00:00:00Z")
		productsInfo = append(productsInfo, productInfo)
		usersListInfo = append(usersListInfo, userListInfo)

	}

	if err := rows.Err(); err != nil {
		return nil, nil, err
	}
	return productsInfo, usersListInfo, nil
}

func UserRecipes(email string) ([]RecipeWithProducts, []Recipe, error) {
	var RecipeInfosWithProducts []RecipeWithProducts
	var RecipeInfos []Recipe

	query := `
		SELECT r.ID, r.Name, r.Description, r.Portion, p.ID, p.Name, p.Base_measure, p.Type_Food, rp.Amount
		FROM users_recipe ul
		JOIN recipe r ON ul.Recipe_ID = r.ID
		JOIN recipe_products rp ON r.ID = rp.Recipe_ID
		JOIN products p ON rp.Product_ID = p.ID
		WHERE ul.User_ID = ?
	`

	rows, err := DB.Query(query, email)
	if err != nil {
		return nil, nil, err
	}
	defer rows.Close()

	recipeMap := make(map[int]*RecipeWithProducts)

	for rows.Next() {
		var recipeID int
		var recipeName string
		var recipeDescription string
		var product Product
		var amount float64
		var portion int

		if err := rows.Scan(
			&recipeID,
			&recipeName,
			&recipeDescription,
			&portion,
			&product.Id,
			&product.Name,
			&product.BaseMeasure,
			&product.TypeFood,
			&amount,
		); err != nil {
			return nil, nil, err
		}

		if _, exists := recipeMap[recipeID]; !exists {
			recipeMap[recipeID] = &RecipeWithProducts{
				Recipe: Recipe{
					Id:          recipeID,
					Name:        recipeName,
					Description: recipeDescription,
					Portion:     portion,
				},
			}
		}

		recipeMap[recipeID].Products = append(recipeMap[recipeID].Products, product)
		recipeMap[recipeID].Amounts = append(recipeMap[recipeID].Amounts, amount)
	}

	for _, recipe := range recipeMap {
		RecipeInfosWithProducts = append(RecipeInfosWithProducts, *recipe)
	}

	// Extract just the Recipe information without products and amounts
	for _, recipe := range RecipeInfosWithProducts {
		RecipeInfos = append(RecipeInfos, recipe.Recipe)
	}

	if err := rows.Err(); err != nil {
		return nil, nil, err
	}

	return RecipeInfosWithProducts, RecipeInfos, nil
}

func InsertUserProduct(sessionToken string, productName, baseMeasure, typeFood string, expirationDate string, amount float64, imageID int, favorite bool) error {
	email, err := GetEmailBySessionToken(sessionToken)
	if err != nil {
		return err
	}

	// Check if the product already exists
	var productID int
	err = DB.QueryRow("SELECT ID FROM products WHERE Name = ? AND Base_measure = ? AND Type_food = ?", productName, baseMeasure, typeFood).Scan(&productID)
	if err != nil {
		if err == sql.ErrNoRows {
			// Product does not exist, insert it and get the newly generated ID
			result, err := DB.Exec("INSERT INTO products (Name, Base_measure, Type_food) VALUES (?, ?, ?)", productName, baseMeasure, typeFood)
			if err != nil {
				return err
			}
			lastInsertID, _ := result.LastInsertId()
			productID = int(lastInsertID)
		} else {
			// Error occurred while checking for existing product
			return err
		}
	}

	// Check if the same User_ID and Product_ID combination already exists
	var existingAmount float64
	err = DB.QueryRow("SELECT Amount FROM users_lists WHERE User_ID = ? AND Product_ID = ? AND Expiration_date = ?", email, productID, expirationDate).Scan(&existingAmount)
	if err == sql.ErrNoRows {
		// Combination does not exist, insert it
		_, err := DB.Exec("INSERT INTO users_lists (User_ID, Product_ID, Expiration_date, Amount, Image_ID, Favorite) VALUES (?, ?, ?, ?, ?, ?)",
			email, productID, expirationDate, amount, imageID, favorite)
		if err != nil {
			return err
		}
	} else if err == nil {
		// Combination exists, update the Amount
		newAmount := existingAmount + amount
		_, err := DB.Exec("UPDATE users_lists SET Amount = ? WHERE User_ID = ? AND Product_ID = ?", newAmount, email, productID)
		if err != nil {
			return err
		}
	} else {
		// Other error occurred while checking for existing combination
		return err
	}

	return nil
}
func InsertUserProductAmount(email string, productID int, amount float64, expirationDate string) error {
	var databaseAmount float64
	err := DB.QueryRow("SELECT Amount FROM users_lists WHERE User_ID = ? AND Product_ID = ? AND Expiration_date = ?", email, productID, expirationDate).Scan(&databaseAmount)
	if err != nil {
		return err
	}
	var newAmount = databaseAmount + amount
	_, err = DB.Exec("UPDATE users_lists set Amount = ? WHERE User_Id = ? AND Product_Id = ? AND Expiration_date = ?", newAmount, email, productID, expirationDate)
	if err != nil {
		return err
	}

	return nil
}

func InsertUserRecipe(sessionToken string, recipeName, description string, portion int, requiredProducts []int, amounts []float64) error {
	email, err := GetEmailBySessionToken(sessionToken)
	if err != nil {
		return err
	}

	// Check if the recipe already exists
	var recipeID int
	err = DB.QueryRow("SELECT ID FROM recipe WHERE Name = ? AND Description = ? AND Portion = ? ", recipeName, description, portion).Scan(&recipeID)
	if err != nil {
		if err == sql.ErrNoRows {
			// Recipe does not exist, insert it and get the newly generated ID
			result, err := DB.Exec("INSERT INTO recipe (Name, Description, Portion) VALUES (?, ?, ?)", recipeName, description, portion)
			if err != nil {
				return err
			}
			lastInsertID, _ := result.LastInsertId()
			recipeID = int(lastInsertID)
		} else {
			// Error occurred while checking for existing recipe
			return err
		}
	}

	// Check if the recipe is already in the user's recipe list
	var exists int
	err = DB.QueryRow("SELECT 1 FROM users_recipe WHERE User_ID = ? AND Recipe_ID = ?", email, recipeID).Scan(&exists)
	if err != nil && err != sql.ErrNoRows {
		return err
	}
	if exists == 0 {
		// Insert the recipe into the user's recipe list
		_, err := DB.Exec("INSERT INTO users_recipe (User_ID, Recipe_ID) VALUES (?, ?)", email, recipeID)
		if err != nil {
			return err
		}
	}

	// Insert required products into the recipe_products junction table
	for i, productID := range requiredProducts {
		// Check if the product is already associated with the recipe
		var productExists int
		err = DB.QueryRow("SELECT 1 FROM recipe_products WHERE Recipe_ID = ? AND Product_ID = ?", recipeID, productID).Scan(&productExists)
		if err != nil && err != sql.ErrNoRows {
			return err
		}
		if productExists == 0 {
			// Insert the product into the recipe_products junction table with the associated Amount
			_, err = DB.Exec("INSERT INTO recipe_products (Recipe_ID, Product_ID, Amount) VALUES (?, ?, ?)", recipeID, productID, amounts[i])
			if err != nil {
				return err
			}
		}

	}

	return nil
}
func EditRecipe(sessionToken string, recipeID int, recipeName string, description string, portion int, requiredProducts []int, amounts []float64) error {
	email, err := GetEmailBySessionToken(sessionToken)
	if err != nil {
		return err
	}

	// Check if the recipe is already in the user's recipe list
	var exists int
	err = DB.QueryRow("SELECT 1 FROM users_recipe WHERE User_ID = ? AND Recipe_ID = ?", email, recipeID).Scan(&exists)
	if err != nil && err != sql.ErrNoRows {
		return err
	}
	if exists == 0 {
		// Recipe does not exist in the user's recipe list
		return errors.New("Recipe not found in the user's recipe list")
	}
	// Execute an UPDATE statement to modify the recipe details
	_, err = DB.Exec("UPDATE recipe SET Name=?, Description = ?, Portion = ? WHERE ID = ?", recipeName, description, portion, recipeID)
	if err != nil {
		return err
	}
	// Iterate through required products and amounts and update the recipe_products table
	for i, productID := range requiredProducts {
		// Update the Amount for the specified Product_ID and Recipe_ID
		_, err := DB.Exec("UPDATE recipe_products SET Amount = ? WHERE Recipe_ID = ? AND Product_ID = ?", amounts[i], recipeID, productID)
		if err != nil {
			return err
		}
	}
	return nil
}

func DeleteUserProduct(sessionToken string, productID int) error {
	email, err := GetEmailBySessionToken(sessionToken)
	if err != nil {
		return err
	}

	// Execute a SQL DELETE statement to remove the product from the user's product list
	query := "DELETE FROM users_lists WHERE User_Id = ? AND Product_Id = ?"

	_, err = DB.Exec(query, email, productID)
	if err != nil {
		return err
	}

	// Check if the product is in other user's lists
	var countUsersLists, countRecipeProducts int

	// Query for users_lists table
	err = DB.QueryRow("SELECT COUNT(*) FROM users_lists WHERE Product_Id = ?", productID).Scan(&countUsersLists)
	if err != nil {
		return err
	}

	// Query for recipe_products table
	err = DB.QueryRow("SELECT COUNT(*) FROM recipe_products WHERE Product_Id = ?", productID).Scan(&countRecipeProducts)
	if err != nil {
		return err
	}

	// Now you can use countUsersLists and countRecipeProducts independently
	totalCount := countUsersLists + countRecipeProducts

	// If the product is not in other user's lists, delete it from the products table
	if totalCount == 0 {
		_, err := DB.Exec("DELETE FROM products WHERE ID = ?", productID)
		if err != nil {
			return err
		}
	}

	return nil
}

func DeleteUserRecipe(sessionToken string, recipeID int) error {
	email, err := GetEmailBySessionToken(sessionToken)
	if err != nil {
		return err
	}

	query := `DELETE FROM main.users_recipe
			  WHERE User_Id = ? AND Recipe_ID = (SELECT ID FROM main.recipe WHERE ID = ?)`

	_, err = DB.Exec(query, email, recipeID)
	if err != nil {
		return err
	}

	return nil
}

func OpenDB() {
	var err error
	DB, err = sql.Open("sqlite3", "fridgeio.db")
	if err != nil {
		fmt.Println("Unable to open database, panicking")
		panic(err)
	}

	err = DB.Ping()
	if err != nil {
		fmt.Println("Unable to connect to database, panicking")
		panic(err)
	}
}

func CloseDB() {
	DB.Close()
}

func OpenTestDB() {
	var err error
	DB, err = sql.Open("sqlite3", "fridgeioTest.db")
	if err != nil {
		fmt.Println(err)
		return
	}
	err = DB.Ping()
	if err != nil {
		fmt.Println("Unable to connect to database, panicking")
		panic(err)
	}
}

func ExecSQL(sql string) {
	_, err := DB.Exec(sql)
	if err != nil {
		log.Fatal(err)
	}
}
func GetSessionToken(email string) (string, error) {

	var sessionToken string
	err := DB.QueryRow("SELECT sessionToken FROM USERS WHERE email = $1", email).Scan(&sessionToken)

	if err == sql.ErrNoRows {
		return "", fmt.Errorf("User not found")
	} else if err != nil {
		return "", err
	}

	return sessionToken, nil
}
func MakeSessionToken(email string) (string, error) {
	tokenLength := 32

	var token string

	for {
		// Create a byte slice to hold the random data
		randomBytes := make([]byte, tokenLength)

		// Read random data from the crypto/rand package
		_, err := io.ReadFull(rand.Reader, randomBytes)
		if err != nil {
			return "3", err
		}

		// Encode the random data to a base64 string
		token = base64.URLEncoding.EncodeToString(randomBytes)

		// Check if the generated token is already in use
		isValid, checkErr := isSessionTokenValid(token)
		if checkErr != nil {
			return "3", checkErr
		}

		if !isValid {
			break
		}
	}

	// Update the user's session token in the database
	_, updateErr := DB.Exec("UPDATE users SET sessionToken = ? WHERE email = ?", token, email)
	if updateErr != nil {
		return "6", updateErr
	}

	return token, nil
}
func isSessionTokenValid(sessionToken string) (bool, error) {
	var exists bool
	err := DB.QueryRow("SELECT EXISTS(SELECT 1 FROM users WHERE sessionToken = ?);", sessionToken).Scan(&exists)
	if err != nil {
		return false, err
	}
	return exists, nil
}
func InsertFavorite(sessionToken string, productID int) error {

	email, err := GetEmailBySessionToken(sessionToken)
	if err != nil {
		return err
	}

	var currentFavorite bool
	err = DB.QueryRow("SELECT Favorite FROM USERS_LISTS WHERE User_ID = ? AND Product_ID = ?", email, productID).Scan(&currentFavorite)
	if err != nil {
		return err
	}

	currentFavorite = !currentFavorite

	_, err = DB.Exec("UPDATE USERS_LISTS SET Favorite = ? WHERE User_ID = ? AND Product_ID = ?", currentFavorite, email, productID)
	if err != nil {
		return err
	}

	return nil
}
func GetEmailBySessionToken(sessionToken string) (string, error) {
	var email string
	err := DB.QueryRow("SELECT Email FROM Users WHERE SessionToken = ?", sessionToken).Scan(&email)
	if err != nil {
		return "", err
	}
	return email, nil
}
