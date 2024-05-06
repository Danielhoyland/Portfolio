package main

import (
	"EgressAPI/handlers"
	"EgressAPI/other"
	"database/sql"
	"fmt"
	"github.com/go-sql-driver/mysql"
	"github.com/joho/godotenv"
	"log"
	"net/http"
	"os"
	"time"
)

var cfg = mysql.Config{
	User:                 "root",
	Passwd:               "",
	Net:                  "tcp",
	Addr:                 "",
	DBName:               "test",
	AllowNativePasswords: true,
	ParseTime:            true,
}

var (
	CertPath = "/etc/letsencrypt/live/powertracker.public.skyhigh.iik.ntnu.no/fullchain.pem"
	KeyPath  = "/etc/letsencrypt/live/powertracker.public.skyhigh.iik.ntnu.no/privkey.pem"
)

func main() {
	err := godotenv.Load()
	if err != nil {
		log.Fatal(err.Error())
	}
	cfg.Passwd = os.Getenv("DB_PASS")
	cfg.Addr = os.Getenv("DB_PATH")
	other.WebsiteURL = os.Getenv("WEB_URL")
	other.ADMIN_WEB_URL = os.Getenv("ADMIN_WEB_URL")

	other.DB, err = sql.Open("mysql", cfg.FormatDSN())
	if err != nil {
		fmt.Println("Error opening database!")
		log.Fatal(err.Error())
	}
	defer func(Db *sql.DB) {
		err_ := Db.Close()
		if err_ != nil {
			log.Fatal(err_.Error())
		}
	}(other.DB)

	pingErr := other.DB.Ping()
	if pingErr != nil {
		log.Fatal(pingErr.Error())
	}
	fmt.Println("Database connected")

	go handlers.ClearSession(time.Duration(24))

	http.HandleFunc("/login", handlers.LoginHandler)

	http.HandleFunc("/overview", handlers.OverviewHandler)

	http.HandleFunc("/sensorGatewayData", handlers.SensorGatewayData)

	http.HandleFunc("/userData", handlers.UserData)

	http.HandleFunc("/buildDep", handlers.BuildDep)

	http.HandleFunc("/process", handlers.Process)

	http.HandleFunc("/processMachine", handlers.ProcessMachine)

	http.HandleFunc("/enoek", handlers.EnoekData)

	http.HandleFunc("/syslogin", handlers.AdminLogin)

	if os.Getenv("DEP") == "FALSE" {
		fmt.Println("Started server listening on port 9091 on development mode")
		log.Fatal(http.ListenAndServe(":9091", nil))
	} else {
		fmt.Println("Started server listening on port 9091 on deployment mode")
		log.Fatal(http.ListenAndServeTLS(":9091", CertPath, KeyPath, nil))
	}
}
