package other

import (
	"database/sql"
	"time"
)

// global variables

var DB *sql.DB
var WebsiteURL string
var ADMIN_WEB_URL = ""

// APIKEY The api key for chirpstack, read from an env var
var APIKEY = ""
var CHIRP_URL = ""
var DEVICE_PROFILE_ID = ""
var TENANT_ID = ""
var APPLICATION_ID = ""

type OutputHotdrop struct {
	Eui           string `json:"devEui"`
	Description   string `json:"description"`
	Name          string `json:"name"`
	Disabled      bool   `json:"isDisabled"`
	SkipFcntCheck bool   `json:"skipFcntCheck"`
	JoinEui       string `json:"joinEui"`
	AppId         string `json:"applicationId"`
	ProfileId     string `json:"deviceProfileId"`
}

type OutputGateway struct {
	Eui           string `json:"gatewayId"`
	Name          string `json:"name"`
	StatsInterval int    `json:"statsInterval"`
	TenantId      string `json:"tenantId"`
}

// Session represents the "session" table.
type Session struct {
	Token     int       `json:"token"`
	UserID    int       `json:"user_id"`
	CreatedAt time.Time `json:"created_at"`
	ExpiresAt time.Time `json:"expires_at"`
}

// Users represents the "users" table.
type Users struct {
	ID         int    `json:"id"`
	Email      string `json:"email"`
	FirstName  string `json:"first_name"`
	LastName   string `json:"last_name"`
	Password   string `json:"password"`
	Permission int    `json:"permission"`
	CompanyID  int    `json:"company_id"`
}

// Company represents the "company" table.
type Company struct {
	ID   int    `json:"id"`
	Name string `json:"name"`
}

// Building represents the "building" table.
type Building struct {
	ID        int    `json:"id"`
	CompanyID int    `json:"company_id"`
	Name      string `json:"name"`
}

// Department represents the "department" table.
type Department struct {
	ID         int    `json:"id"`
	BuildingID int    `json:"building_id"`
	Name       string `json:"name"`
}

// Machine represents the "machine" table.
type Machine struct {
	EUI          string `json:"eui"`
	Name         string `json:"name"`
	ExpectedUse  int    `json:"expected_use"`
	MachineNr    string `json:"machine_nr"`
	BuildingID   int    `json:"building_id"`
	DepartmentID int    `json:"department_id"`
	Voltage      int    `json:"voltage"`
}

// EnokSuggestionMeasures represents the "enoek_suggestion_measures" table.
type EnokSuggestionMeasures struct {
	ID          int       `json:"id"`
	Header      string    `json:"header"`
	Description string    `json:"description"`
	Author      int       `json:"author"`
	StartDate   time.Time `json:"start_date"`
	StopDate    time.Time `json:"stop_date"`
	Active      bool      `json:"active"`
	ProcessID   int       `json:"process_id"`
	Approved    *bool     `json:"approved"`
}

// Processes represents the "processes" table.
type Processes struct {
	ID          int    `json:"id"`
	Name        string `json:"name"`
	Description string `json:"description"`
	CompanyID   int    `json:"comp_id"`
}

// MachineProcesses represents the "machine_processes" table.
type MachineProcesses struct {
	MachineID   string `json:"machine_id"`
	ProcessesID int    `json:"processes_id"`
}

// SensorData represents the "sensorData" table.
type SensorData struct {
	EUI          string    `json:"eui"`
	Accumulation float64   `json:"accumulation"`
	AVGCurrent   float64   `json:"avg_current"`
	OffsetMax    float64   `json:"offset_max"`
	OffsetMin    float64   `json:"offset_min"`
	Voltage      float64   `json:"voltage"`
	Temperature  float64   `json:"temperature"`
	DateTime     time.Time `json:"date_time"`
}
type Gateway struct {
	EUI       string `json:"eui_gate"`
	Name      string `json:"name"`
	CompanyID int    `json:"comp_id"`
}
