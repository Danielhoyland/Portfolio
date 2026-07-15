package database

import (
	"time"
)

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
}

// Processes represents the "processes" table.
type Processes struct {
	ID          int    `json:"id"`
	Name        string `json:"name"`
	Description string `json:"description"`
}

// MachineProcesses represents the "machine_processes" table.
type MachineProcesses struct {
	MachineID   string `json:"machine_id"`
	ProcessesID int    `json:"processes_id"`
}

// SensorData represents the "sensorData" table.
type SensorData struct {
	EUI         string    `json:"eui"`
	Acumulation int       `json:"acumulation"`
	AVGCurrent  int       `json:"avg_current"`
	OffsetMax   int       `json:"offset_max"`
	OffsetMin   int       `json:"offset_min"`
	Voltage     int       `json:"voltage"`
	Temperature int       `json:"temperature"`
	DateTime    time.Time `json:"date_time"`
}
