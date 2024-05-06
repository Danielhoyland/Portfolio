package other

import (
	"database/sql"
	"time"
)

var DB *sql.DB
var WebsiteURL string
var ADMIN_WEB_URL string

// UserInfo This struct is used for both logging in and for authenticating user (with session token)
type UserInfo struct {
	Username     string `json:"uname"`
	Password     string `json:"pword"`
	SessionToken string `json:"stoken"`
}

// IncomingRequest This struct is the top level for ALL requests coming from frontend, two members, user info and the body
type IncomingRequest struct {
	Uinfo UserInfo    `json:"uinfo"`
	Body  interface{} `json:"body"`
}

// Session represents the "session" table.
type Session struct {
	Token     string    `json:"token"`
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

type EnokSuggestionMeasures2 struct {
	ID          int       `json:"id"`
	Header      string    `json:"header"`
	Description string    `json:"description"`
	Author      string    `json:"author"`
	StartDate   time.Time `json:"start_date"`
	StopDate    time.Time `json:"stop_date"`
	Active      bool      `json:"active"`
	Process     string    `json:"process"`
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

type SensorData struct {
	EUI         string    `json:"eui"`
	Acumulation float64   `json:"acumulation"`
	AVGCurrent  float64   `json:"avg_current"`
	OffsetMax   float64   `json:"offset_max"`
	OffsetMin   float64   `json:"offset_min"`
	Voltage     float64   `json:"voltage"`
	Temperature float64   `json:"temperature"`
	DateTime    time.Time `json:"date_time"`
}
type SensorInfo struct {
	EUI            string  `json:"eui"`
	MachineName    string  `json:"machine_name"`
	ExpectedUse    int     `json:"expected_use"`
	MachineNr      string  `json:"machineNr"`
	BuildingID     int     `json:"building_id"`
	BuildingName   string  `json:"building_name"`
	DepartmentID   int     `json:"department_id"`
	DepartmentName *string `json:"department_name"`
	Voltage        int     `json:"voltage"`
}
type MachineDetails struct {
	Machine             Machine      `json:"machine"`
	Building            Building     `json:"building"`
	Department          Department   `json:"department"`
	SensorDataList      []SensorData `json:"sensor_data_list"`
	AssociatedProcesses []Processes  `json:"associated_processes"`
}

type Profile struct {
	ID         int    `json:"id"`
	Email      string `json:"email"`
	FirstName  string `json:"first_name"`
	LastName   string `json:"last_name"`
	Permission int    `json:"permission"`
}

type Gateway struct {
	EUI       string  `json:"eui_gate"`
	Name      *string `json:"name"`
	CompanyID int     `json:"comp_id"`
}

type SimpleMachine struct {
	EUI  string `json:"eui"`
	Name string `json:"name"`
}

// structure of dataset on frontend
type Dataset struct {
	Label string    `json:"label"`
	Data  []float64 `json:"data"`
}

type OverviewData struct {
	Buildings []OBuilding `json:"buildings"`
	Processes []OProcess  `json:"processes"`
	Labels    []time.Time `json:"labels"`
}

type OBuilding struct {
	Name        string        `json:"name"`
	Departments []ODepartment `json:"departments"`
	Dataset     Dataset       `json:"dataset"`
}

type ODepartment struct {
	Name     string     `json:"name"`
	Machines []OMachine `json:"machines"`
	Dataset  Dataset    `json:"dataset"`
}

type OMachine struct {
	Name    string  `json:"name"`
	EUI     string  `json:"eui"`
	Dataset Dataset `json:"dataset"`
}

type OProcess struct {
	Name     string     `json:"name"`
	Machines []OMachine `json:"machines"`
	Dataset  Dataset    `json:"dataset"`
}
