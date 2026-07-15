package main

import (
	"API/database"
	"API/handlers"
	_ "github.com/mattn/go-sqlite3"
	"log"
	"net/http"
	"os"
)

func main() {

	database.OpenDB()
	defer database.CloseDB()

	port := os.Getenv("PORT")
	if port == "" {
		log.Println("Port not found, starting default, 8080...")
		port = "8080"
	}

	http.HandleFunc("/getData", handlers.GetData)
	http.HandleFunc("/new-user", handlers.NewUserHandler)
	http.HandleFunc("/new-item", handlers.AddItem)
	http.HandleFunc("/new-recipe", handlers.AddRecipe)
	http.HandleFunc("/get-recipe", handlers.GetRecipes)
	http.HandleFunc("/change-recipe", handlers.EditRecipe)
	http.HandleFunc("/sessionToken", handlers.MakeSessionTokenHandler)
	http.HandleFunc("/setFavorite", handlers.SetFavorite)
	http.HandleFunc("/changeAmount", handlers.ChangeAmountHandler)

	log.Println("Starting server on port " + port + " ...")
	log.Fatal(http.ListenAndServe(":"+port, nil))
}

// Used to initialize a new (empty) database, remember to remove function call after first run
func initDatabase() {
	sqlDump, err := os.ReadFile("fresh_database.txt")
	if err != nil {
		log.Fatal(err)
	}

	statements := string(sqlDump)
	database.ExecSQL(statements)
}
