package test

import (
	"EgressAPI/other"
	"database/sql"
	"fmt"
	"github.com/go-sql-driver/mysql"
	"log"
	"reflect"
	"testing"
)

var cfg = mysql.Config{
	User:                 "root",
	Passwd:               "", //enter password
	Net:                  "tcp",
	Addr:                 "localhost:3306",
	DBName:               "test",
	AllowNativePasswords: true,
}

func OpenDB() {
	var err error
	other.DB, err = sql.Open("mysql", cfg.FormatDSN())
	if err != nil {
		fmt.Println("Error opening database!")
		log.Fatal(err.Error())
	}
	/*defer func(Db *sql.DB) {
		err := Db.Close()
		if err != nil {
			log.Fatal(err.Error())
		}
	}(other.DB)*/

	pingErr := other.DB.Ping()
	if pingErr != nil {
		fmt.Println("Database failed to respond")
		log.Println(pingErr.Error())
	}
	fmt.Println("Database connected")
}
func TestGetPermission(t *testing.T) {
	OpenDB()
	permission, err := other.GetPermissionFromToken("0321412")
	if err != nil {
		t.Errorf("Error: %v", err)
		return
	}

	expectedPermission := 0
	if permission != expectedPermission {
		t.Errorf("Expected permission %d, but got %d", expectedPermission, permission)
	}
}
func TestGetUsers(t *testing.T) {
	OpenDB()
	users, err := other.SeeUsers("0321412")
	if err != nil {
		t.Errorf("Error: %v", err)
		return
	}
	expectedUserCount := 1
	if len(users) != expectedUserCount {
		t.Errorf("Expected %d users, but got %d", expectedUserCount, len(users))
	}
	expectedUser := other.Users{
		Email:      "admin@admin.no",
		FirstName:  "admin",
		LastName:   "admin",
		Password:   "admin",
		Permission: 0, //highest premission = 0
		CompanyID:  1,
	}
	firstUser := users[0]
	if firstUser.Email != expectedUser.Email ||
		firstUser.FirstName != expectedUser.FirstName ||
		firstUser.LastName != expectedUser.LastName ||
		firstUser.Password != expectedUser.Password ||
		firstUser.Permission != expectedUser.Permission ||
		firstUser.CompanyID != expectedUser.CompanyID {
		t.Errorf("Expected user values do not match actual values")
	}
}
func TestGetSensors(t *testing.T) {
	OpenDB()
	sensors, err := other.GetSensors("0321412")
	if err != nil {
		t.Errorf("Error: %v", err)
		return
	}
	expectedSensorCount := 1
	if len(sensors) != expectedSensorCount {
		t.Errorf("Expected %d users, but got %d", expectedSensorCount, len(sensors))
	}
	expectedSensors := other.SensorInfo{
		EUI:            "E890E1CC8CF44B49",
		MachineName:    "press",
		ExpectedUse:    5,
		MachineNr:      "4727AD",
		BuildingID:     0,
		BuildingName:   "A",
		DepartmentID:   0,
		DepartmentName: "AA",
	}
	firstSensor := sensors[0]
	if firstSensor.EUI != expectedSensors.EUI ||
		firstSensor.MachineName != expectedSensors.MachineName ||
		firstSensor.ExpectedUse != expectedSensors.ExpectedUse ||
		firstSensor.MachineNr != expectedSensors.MachineNr ||
		firstSensor.BuildingID != expectedSensors.BuildingID ||
		firstSensor.BuildingName != expectedSensors.BuildingName ||
		firstSensor.DepartmentID != expectedSensors.DepartmentID ||
		firstSensor.DepartmentName != expectedSensors.DepartmentName {
		t.Errorf("Expected user values do not match actual values")
	}
}

// gets enoek with activity bool = false
func TestGetEnokSuggestions(t *testing.T) {
	OpenDB()
	enoek, err := other.GetEnokSuggestions("0321412")
	if err != nil {
		t.Errorf("Error: %v", err)
		return
	}
	expectedEnoekCount := 1
	if len(enoek) != expectedEnoekCount {
		t.Errorf("Expected %d users, but got %d", expectedEnoekCount, len(enoek))
	}
	expectedEnoek := other.EnokSuggestionMeasures{
		Header:      "Oven",
		Description: "Oven burn more stuff",
		Author:      1,
		StartDate:   "2024-02-01",
		StopDate:    "2024-07-09",
		ProcessID:   1,
	}
	firstEnoek := enoek[0]
	if firstEnoek.Header != expectedEnoek.Header ||
		firstEnoek.Description != expectedEnoek.Description ||
		firstEnoek.Author != expectedEnoek.Author ||
		firstEnoek.StartDate != expectedEnoek.StartDate ||
		firstEnoek.StopDate != expectedEnoek.StopDate ||
		firstEnoek.ProcessID != expectedEnoek.ProcessID {
		t.Errorf("Expected enøk suggestion values do not match actual values")
	}
}

// gets enoek with activity bool = true
func TestGetEnokMeasures(t *testing.T) {
	OpenDB()
	enoek, err := other.GetEnokMeasures("0321412")
	if err != nil {
		t.Errorf("Error: %v", err)
		return
	}
	expectedEnoekCount := 1
	if len(enoek) != expectedEnoekCount {
		t.Errorf("Expected %d users, but got %d", expectedEnoekCount, len(enoek))
	}
	expectedEnoek := other.EnokSuggestionMeasures{
		Header:      "TestHeader",
		Description: "TestDescription",
		Author:      1,
		StartDate:   "2024-02-20",
		StopDate:    "2024-02-25",
		ProcessID:   1,
	}
	firstEnoek := enoek[0]
	if firstEnoek.Header != expectedEnoek.Header ||
		firstEnoek.Description != expectedEnoek.Description ||
		firstEnoek.Author != expectedEnoek.Author ||
		firstEnoek.StartDate != expectedEnoek.StartDate ||
		firstEnoek.StopDate != expectedEnoek.StopDate ||
		firstEnoek.ProcessID != expectedEnoek.ProcessID {
		t.Errorf("Expected enøk suggestion values do not match actual values")
	}
}
func TestGetAllEnok(t *testing.T) {
	OpenDB()
	enoek, err := other.GetAllEnoek("0321412")
	if err != nil {
		t.Errorf("Error: %v", err)
		return
	}
	expectedEnoekCount := 2
	if len(enoek) != expectedEnoekCount {
		t.Errorf("Expected %d users, but got %d", expectedEnoekCount, len(enoek))
	}
	expectedEnoek := other.EnokSuggestionMeasures{
		Header:      "Oven",
		Description: "Oven burn more stuff",
		Author:      1,
		StartDate:   "2024-02-01",
		StopDate:    "2024-07-09",
		ProcessID:   1,
	}
	firstEnoek := enoek[0]
	if firstEnoek.Header != expectedEnoek.Header ||
		firstEnoek.Description != expectedEnoek.Description ||
		firstEnoek.Author != expectedEnoek.Author ||
		firstEnoek.StartDate != expectedEnoek.StartDate ||
		firstEnoek.StopDate != expectedEnoek.StopDate ||
		firstEnoek.ProcessID != expectedEnoek.ProcessID {
		t.Errorf("Expected enøk suggestion values do not match actual values")
	}
	expectedEnoek = other.EnokSuggestionMeasures{
		Header:      "TestHeader",
		Description: "TestDescription",
		Author:      1,
		StartDate:   "2024-02-20",
		StopDate:    "2024-02-25",
		ProcessID:   1,
	}
	firstEnoek = enoek[1]
	if firstEnoek.Header != expectedEnoek.Header ||
		firstEnoek.Description != expectedEnoek.Description ||
		firstEnoek.Author != expectedEnoek.Author ||
		firstEnoek.StartDate != expectedEnoek.StartDate ||
		firstEnoek.StopDate != expectedEnoek.StopDate ||
		firstEnoek.ProcessID != expectedEnoek.ProcessID {
		t.Errorf("Expected enøk suggestion values do not match actual values")
	}
}
func TestGetMachineDetailsForUser(t *testing.T) {
	OpenDB()
	machines, err := other.GetMachineDetailsForUser("0321412")
	if err != nil {
		t.Errorf("Error: %v", err)
		return
	}
	expectedMachineCount := 2
	if len(machines) != expectedMachineCount {
		t.Errorf("Expected %d users, but got %d", expectedMachineCount, len(machines))
	}
	expectedMachineDetails := other.MachineDetails{
		Machine: other.Machine{
			EUI:          "E890E1CC8CF44B49",
			Name:         "press",
			ExpectedUse:  5,
			MachineNr:    "4727AD",
			BuildingID:   1,
			DepartmentID: 1,
		},
		Building: other.Building{
			ID:        1,
			CompanyID: 1,
			Name:      "A",
		},
		Department: other.Department{
			ID:         1,
			BuildingID: 1,
			Name:       "AA",
		},
		SensorDataList: []other.SensorData{
			{
				EUI:         "E890E1CC8CF44B49",
				Acumulation: 0,
				AVGCurrent:  0,
				OffsetMax:   0,
				OffsetMin:   0,
				Voltage:     0,
				Temperature: 0,
				//DateTime:    "2024-02-11 15:30:45",
			},
		},
		AssociatedProcesses: []other.Processes{
			{
				ID:          1,
				Name:        "WINDOW",
				Description: "Make window for houses",
				CompanyID:   1,
			},
		},
	}

	firstMachineDetails := machines[1]
	if firstMachineDetails.Machine.EUI != expectedMachineDetails.Machine.EUI ||
		firstMachineDetails.Machine.Name != expectedMachineDetails.Machine.Name ||
		firstMachineDetails.Machine.ExpectedUse != expectedMachineDetails.Machine.ExpectedUse ||
		firstMachineDetails.Machine.MachineNr != expectedMachineDetails.Machine.MachineNr ||
		firstMachineDetails.Machine.BuildingID != expectedMachineDetails.Machine.BuildingID ||
		firstMachineDetails.Machine.DepartmentID != expectedMachineDetails.Machine.DepartmentID ||
		firstMachineDetails.Building.ID != expectedMachineDetails.Building.ID ||
		firstMachineDetails.Building.CompanyID != expectedMachineDetails.Building.CompanyID ||
		firstMachineDetails.Building.Name != expectedMachineDetails.Building.Name ||
		firstMachineDetails.Department.ID != expectedMachineDetails.Department.ID ||
		firstMachineDetails.Department.BuildingID != expectedMachineDetails.Department.BuildingID ||
		firstMachineDetails.Department.Name != expectedMachineDetails.Department.Name ||
		!reflect.DeepEqual(firstMachineDetails.SensorDataList, expectedMachineDetails.SensorDataList) ||
		!reflect.DeepEqual(firstMachineDetails.AssociatedProcesses, expectedMachineDetails.AssociatedProcesses) {
		t.Errorf("Expected machine details values do not match actual values")
	}
}
func TestGetBuildingsAndDepartments(t *testing.T) {
	OpenDB()
	buildingsAndDep, err := other.GetBuildingsAndDepartments("0321412")
	if err != nil {
		t.Errorf("Error: %v", err)
		return
	}
	type BuildingDepartmentMap map[other.Building][]other.Department
	expectedOutput := BuildingDepartmentMap{
		other.Building{ID: 1, CompanyID: 0, Name: "A"}: []other.Department{
			{ID: 1, BuildingID: 0, Name: "AA"},
			{ID: 2, BuildingID: 0, Name: "AB"},
			{ID: 3, BuildingID: 0, Name: "AC"},
		},
		other.Building{ID: 2, CompanyID: 0, Name: "B"}: []other.Department{
			{ID: 4, BuildingID: 0, Name: "BA"},
			{ID: 5, BuildingID: 0, Name: "BB"},
			{ID: 6, BuildingID: 0, Name: "BC"},
		},
		other.Building{ID: 3, CompanyID: 0, Name: "C"}: []other.Department{
			{ID: 7, BuildingID: 0, Name: "CA"},
			{ID: 8, BuildingID: 0, Name: "CB"},
			{ID: 9, BuildingID: 0, Name: "CC"},
		},
	}

	// Check each building and department
	for building, actualDepartments := range buildingsAndDep {
		expectedDepartments, found := expectedOutput[building]
		if !found {
			t.Errorf("Unexpected building: %+v", building)
			continue
		}

		// Compare departments
		for i, actualDepartment := range actualDepartments {
			if i >= len(expectedDepartments) {
				t.Errorf("Extra department found for building %+v: %+v", building, actualDepartment)
				continue
			}

			expectedDepartment := expectedDepartments[i]

			// Compare individual fields
			if actualDepartment.ID != expectedDepartment.ID ||
				actualDepartment.BuildingID != expectedDepartment.BuildingID ||
				actualDepartment.Name != expectedDepartment.Name {
				t.Errorf("Mismatch in department data for building %+v: expected %+v, got %+v", building, expectedDepartment, actualDepartment)
			}
		}

		// Check if there are any remaining-expected departments
		if len(actualDepartments) < len(expectedDepartments) {
			t.Errorf("Expected departments not found for building %+v", building)
		}
	}
}
func TestGetProfile(t *testing.T) {
	OpenDB()
	profile, err := other.GetProfile("0321412")
	if err != nil {
		t.Errorf("Error: %v", err)
		return
	}
	expectedProfileCount := 1
	if len(profile) != expectedProfileCount {
		t.Errorf("Expected %d users, but got %d", expectedProfileCount, len(profile))
	}
	expectedSensors := other.Profile{
		Email:     "admin@admin.no",
		FirstName: "admin",
		LastName:  "admin",
	}
	firstProfile := profile[0]
	if firstProfile.Email != expectedSensors.Email ||
		firstProfile.FirstName != expectedSensors.FirstName ||
		firstProfile.LastName != expectedSensors.LastName {
		t.Errorf("Expected user values do not match actual values")
	}
}
func TestGetGateway(t *testing.T) {
	OpenDB()

	token := "0321412"

	Gateways, err := other.GetGateway(token)
	if err != nil {
		t.Errorf("Error: %v", err)
		return
	}
	expectedGatewayCount := 1
	if len(Gateways) != expectedGatewayCount {
		t.Errorf("Expected %d users, but got %d", expectedGatewayCount, len(Gateways))
	}
	expectedGateways := other.Gateway{
		EUI:       "142ee803b99e06d2",
		CompanyID: 1,
	}
	firstGateway := Gateways[0]
	if firstGateway.EUI != expectedGateways.EUI ||
		firstGateway.CompanyID != expectedGateways.CompanyID {
		// Data doesn't match, handle the mismatch
		t.Errorf("Inserted data doesn't match expected values")
	}
}
func TestGetProcess(t *testing.T) {
	OpenDB()

	token := "0321412"

	processes, err := other.GetProcess(token)
	if err != nil {
		t.Errorf("Error: %v", err)
		return
	}
	expectedProcessCount := 1
	if len(processes) != expectedProcessCount {
		t.Errorf("Expected %d users, but got %d", expectedProcessCount, len(processes))
	}
	expectedProcess := other.Processes{
		ID:          1,
		Name:        "WINDOW",
		Description: "Make window for houses",
		CompanyID:   1,
	}
	firstGateway := processes[0]
	if firstGateway.ID != expectedProcess.ID ||
		firstGateway.Name != expectedProcess.Name ||
		firstGateway.Description != expectedProcess.Description ||
		firstGateway.CompanyID != expectedProcess.CompanyID {
		// Data doesn't match, handle the mismatch
		t.Errorf("Inserted data doesn't match expected values")
	}
}
func TestGetBuilding(t *testing.T) {
	OpenDB()

	token := "0321412"

	buildings, err := other.GetBuilding(token)
	if err != nil {
		t.Errorf("Error: %v", err)
		return
	}
	expectedBuildingCount := 3
	if len(buildings) != expectedBuildingCount {
		t.Errorf("Expected %d users, but got %d", expectedBuildingCount, len(buildings))
	}
	expectedBuildings := other.Building{
		ID:        1,
		CompanyID: 1,
		Name:      "A",
	}
	firstBuilding := buildings[0]
	if firstBuilding.ID != expectedBuildings.ID ||
		firstBuilding.CompanyID != expectedBuildings.CompanyID ||
		firstBuilding.Name != expectedBuildings.Name {
		// Data doesn't match, handle the mismatch
		t.Errorf("Inserted data doesn't match expected values")
	}
}
