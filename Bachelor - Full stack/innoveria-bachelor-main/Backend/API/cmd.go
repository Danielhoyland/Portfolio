package main

import (
	"API/handlers"
	"API/other"
	"database/sql"
	"fmt"
	"github.com/go-sql-driver/mysql"
	"github.com/joho/godotenv"
	"log"
	"net/http"
	"os"
)

var cfg = mysql.Config{
	User:                 "root",
	Passwd:               "", //enter password
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
	Init()

	pingErr := other.DB.Ping()
	if pingErr != nil {
		fmt.Println("Database failed to respond")
		log.Fatal(pingErr.Error())
		return
	}
	defer func(Db *sql.DB) {
		err := Db.Close()
		if err != nil {
			log.Fatal(err.Error())
		}
	}(other.DB)
	fmt.Println("Database connected")

	//http.HandleFunc("/req", handlers.Request)		//unused endpoint
	http.HandleFunc("/hotdrop", handlers.HotDropData)
	http.HandleFunc("/new-hotdrop", handlers.NewHotDrop)
	http.HandleFunc("/add-gateway", handlers.AddGateway)
	http.HandleFunc("/new-user", handlers.NewUser)
	http.HandleFunc("/new-building", handlers.NewBuilding)
	http.HandleFunc("/new-department", handlers.NewDepartment)
	http.HandleFunc("/new-process", handlers.NewProcess)
	http.HandleFunc("/machineProcess", handlers.ProcessMachine)
	http.HandleFunc("/new-enoek", handlers.NewEnoek)
	http.HandleFunc("/new-company", handlers.NewCompany)

	if os.Getenv("DEP") == "FALSE" {
		fmt.Println("Started server listening on port 9090 on development mode")
		log.Fatal(http.ListenAndServe(":9090", nil))
	} else {
		fmt.Println("Started server listening on port 9090 on deployment mode")
		log.Fatal(http.ListenAndServeTLS(":9090", CertPath, KeyPath, nil))
	}
}

func Init() {
	err := godotenv.Load()

	if err != nil {
		log.Fatal(err.Error())
		return
	}
	cfg.Passwd = os.Getenv("DB_PASS")
	cfg.Addr = os.Getenv("DB_PATH")
	other.WebsiteURL = os.Getenv("WEB_URL")
	other.APIKEY = os.Getenv("API_KEY")
	other.CHIRP_URL = os.Getenv("CHIRP_URL")
	other.DEVICE_PROFILE_ID = os.Getenv("DEVICE_PROFILE_ID")
	other.APPLICATION_ID = os.Getenv("APPLICATION_ID")
	other.TENANT_ID = os.Getenv("TENANT_ID")
	other.ADMIN_WEB_URL = os.Getenv("ADMIN_WEB_URL")

	other.DB, err = sql.Open("mysql", cfg.FormatDSN())
	if err != nil {
		fmt.Println("Error opening database!")
		log.Fatal(err.Error())
	}
}
