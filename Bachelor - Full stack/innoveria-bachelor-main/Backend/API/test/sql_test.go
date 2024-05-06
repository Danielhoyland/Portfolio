package test

import (
	"API/other"
	"database/sql"
	"fmt"
	"github.com/go-sql-driver/mysql"
	"log"
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
func getCount(tableName string) (int, error) {
	var count int
	err := other.DB.QueryRow("SELECT COUNT(*) FROM " + tableName).Scan(&count)
	return count, err
}

func TestInsertUser(t *testing.T) {
	OpenDB()
	token := "0321412"
	expectedUser := other.Users{
		Email:      "enoekSuper@gmail.com",
		FirstName:  "enoek",
		LastName:   "Super",
		Password:   "123",
		Permission: 1,
	}

	var exists bool
	err := other.DB.QueryRow(`
	SELECT EXISTS(
		SELECT 1
		FROM users
		WHERE email = ?
		  AND first_name = ?
		  AND last_name = ?
	)`, expectedUser.Email, expectedUser.FirstName, expectedUser.LastName).Scan(&exists)

	if exists {
		_, err = other.DB.Exec(`
	 DELETE FROM users
	  WHERE email = ?
		  AND first_name = ?
		  AND last_name = ?`,
			expectedUser.Email, expectedUser.FirstName, expectedUser.LastName)
		if err != nil {
			t.Errorf("Error: %v", err)
			return
		}
	}
	err = other.InsertUser(token, expectedUser.Email, expectedUser.FirstName, expectedUser.LastName, expectedUser.Password, expectedUser.Permission)
	if err != nil {
		t.Errorf("Error: %v", err)
		return
	}
	var insertedData other.Users
	err = other.DB.QueryRow(`
	SELECT *
	FROM users
	  WHERE email = ?
		  AND first_name = ?
		  AND last_name = ?`,
		expectedUser.Email, expectedUser.FirstName, expectedUser.LastName).Scan(
		&insertedData.ID,
		&insertedData.Email,
		&insertedData.FirstName,
		&insertedData.LastName,
		&insertedData.Password,
		&insertedData.Permission,
		&insertedData.CompanyID,
	)
	if err != nil {
		t.Errorf("Error: %v", err)
		return
	}
	userID, err := other.GetUserIDFromToken(token)
	if err != nil {
		t.Errorf("Error: %v", err)
		return
	}
	compID, err := other.GetCompanyFromUserID(userID)
	if err != nil {
		t.Errorf("Error: %v", err)
		return
	}
	if insertedData.Email != expectedUser.Email ||
		insertedData.FirstName != expectedUser.FirstName ||
		insertedData.LastName != expectedUser.LastName ||
		insertedData.Password != expectedUser.Password ||
		insertedData.Permission != expectedUser.Permission ||
		insertedData.CompanyID != compID {
		// Data doesn't match, handle the mismatch
		t.Errorf("Inserted data doesn't match expected values")
	}
}
func TestEditUser(t *testing.T) {
	OpenDB()
	//token := "0321412"
	expectedUser := other.Users{
		ID:         1,
		Email:      "admin@admin.no",
		FirstName:  "admin",
		LastName:   "admin",
		Password:   "admin1",
		Permission: 0,
	}
	/*
		var exists bool
		err := other.DB.QueryRow(`
		SELECT EXISTS(
			SELECT 1
			FROM users
			WHERE email = ?
			  AND first_name = ?
			  AND last_name = ?
		)`, expectedUser.Email, expectedUser.FirstName, expectedUser.LastName).Scan(&exists)

		if !exists {
			err = other.InsertUser(token, expectedUser.Email, expectedUser.FirstName, expectedUser.LastName, expectedUser.Password, expectedUser.Permission)
			if err != nil {
				t.Errorf("Error: %v", err)
				return
			}
		}
	*/
	// Modify user data
	modifiedUser := other.Users{
		ID:         1,
		Email:      "admin@admin.no",
		FirstName:  "admin",
		LastName:   "admin",
		Password:   "admin",
		Permission: 0,
	}

	err := other.EditUser(expectedUser.ID, modifiedUser.Email, modifiedUser.FirstName, modifiedUser.LastName, modifiedUser.Password, modifiedUser.Permission)
	if err != nil {
		t.Errorf("Error editing user: %v", err)
		return
	}
	/*
		// Retrieve edited user data
		var editedData other.Users
		err = other.DB.QueryRow(`
		SELECT *
		FROM users
		  WHERE email = ?
			  AND first_name = ?
			  AND last_name = ?`,
			modifiedUser.Email, modifiedUser.FirstName, modifiedUser.LastName).Scan(
			&editedData.ID,
			&editedData.Email,
			&editedData.FirstName,
			&editedData.LastName,
			&editedData.Password,
			&editedData.Permission,
			&editedData.CompanyID,
		)
		if err != nil {
			t.Errorf("Error retrieving edited user data: %v", err)
			return
		}

		// Compare edited user data
		if editedData.Email != modifiedUser.Email ||
			editedData.FirstName != modifiedUser.FirstName ||
			editedData.LastName != modifiedUser.LastName ||
			editedData.Password != modifiedUser.Password ||
			editedData.Permission != modifiedUser.Permission {
			t.Errorf("Edited user data doesn't match expected values")
		}
		err = other.EditUser(expectedUser.ID, expectedUser.Email, expectedUser.FirstName, expectedUser.LastName, expectedUser.Password, expectedUser.Permission)
		if err != nil {
			t.Errorf("Error editing user: %v", err)
			return
		}

		err = other.DB.QueryRow(`
		SELECT *
		FROM users
		  WHERE email = ?
			  AND first_name = ?
			  AND last_name = ?`,
			expectedUser.Email, expectedUser.FirstName, expectedUser.LastName).Scan(
			&editedData.ID,
			&editedData.Email,
			&editedData.FirstName,
			&editedData.LastName,
			&editedData.Password,
			&editedData.Permission,
			&editedData.CompanyID,
		)
		if err != nil {
			t.Errorf("Error retrieving edited user data: %v", err)
			return
		}

		// Compare edited user data
		if editedData.Email != expectedUser.Email ||
			editedData.FirstName != expectedUser.FirstName ||
			editedData.LastName != expectedUser.LastName ||
			editedData.Password != expectedUser.Password ||
			editedData.Permission != expectedUser.Permission {
			t.Errorf("Edited user data doesn't match expected values")
		}*/
}
func TestDeleteUser(t *testing.T) {
	OpenDB()

	expectedUser := other.Users{
		ID:         3,
		Email:      "enoekSuper@gmail.com",
		FirstName:  "enoek",
		LastName:   "Super",
		Password:   "123",
		Permission: 1,
	}

	/*var exists bool
	err := other.DB.QueryRow(`
	SELECT EXISTS(
		SELECT 1
		FROM users
		WHERE email = ?
		  AND first_name = ?
		  AND last_name = ?
	)`, expectedUser.Email, expectedUser.FirstName, expectedUser.LastName).Scan(&exists)

	if !exists {
		err = other.InsertUser(token, expectedUser.Email, expectedUser.FirstName, expectedUser.LastName, expectedUser.Password, expectedUser.Permission)
		if err != nil {
			t.Errorf("Error: %v", err)
			return
		}
	}*/
	err := other.DeleteUser(expectedUser.ID)
	if err != nil {
		t.Errorf("Error deleting user: %v", err)
		return
	}
	/*var deletedData other.Users
	err = other.DB.QueryRow(`
	SELECT *
	FROM users
	  WHERE email = ?
		  AND first_name = ?
		  AND last_name = ?`,
		expectedUser.Email, expectedUser.FirstName, expectedUser.LastName).Scan(
		&deletedData.ID,
		&deletedData.Email,
		&deletedData.FirstName,
		&deletedData.LastName,
		&deletedData.Password,
		&deletedData.Permission,
		&deletedData.CompanyID,
	)

	if err == nil {
		t.Errorf("User data still exists after deletion")
	}*/
}

func TestInsertBuilding(t *testing.T) {
	OpenDB()
	token := "0321412"
	buildingName := "NewBuilding"

	// Get initial count of buildings
	initialCount, err := getCount("building")
	if err != nil {
		t.Errorf("Error getting initial building count: %v", err)
		return
	}

	// Insert a new building
	err = other.InsertBuilding(token, buildingName)
	if err != nil {
		t.Errorf("Error inserting building: %v", err)
		return
	}

	// Get updated count of buildings
	updatedCount, err := getCount("building")
	if err != nil {
		t.Errorf("Error getting updated building count: %v", err)
		return
	}

	// Check if the count has increased by 1
	if updatedCount != initialCount+1 {
		t.Errorf("Expected building count to increase by 1, but got %d (initial: %d, updated: %d)", updatedCount-initialCount, initialCount, updatedCount)
	}
}
func TestEditBuilding(t *testing.T) {
	OpenDB()
	buildingID := 4
	newBuildingName := "UpdatedBuilding"

	// Get initial building name
	var initialName string
	err := other.DB.QueryRow("SELECT name FROM building WHERE id=?", buildingID).Scan(&initialName)
	if err != nil {
		t.Errorf("Error getting initial building name: %v", err)
		return
	}

	// Edit the building name
	err = other.EditBuilding(buildingID, newBuildingName)
	if err != nil {
		t.Errorf("Error editing building: %v", err)
		return
	}

	// Get updated building name
	var updatedName string
	err = other.DB.QueryRow("SELECT name FROM building WHERE id=?", buildingID).Scan(&updatedName)
	if err != nil {
		t.Errorf("Error getting updated building name: %v", err)
		return
	}

	// Check if the name has been updated
	if updatedName != newBuildingName {
		t.Errorf("Expected building name to be %s, but got %s", newBuildingName, updatedName)
	}
}
func TestDeleteBuilding(t *testing.T) {
	OpenDB()
	buildingID := 4

	// Get initial count of buildings
	initialCount, err := getCount("building")
	if err != nil {
		t.Errorf("Error getting initial building count: %v", err)
		return
	}

	// Delete a building
	err = other.DeleteBuilding(buildingID)
	if err != nil {
		t.Errorf("Error deleting building: %v", err)
		return
	}

	// Get updated count of buildings
	updatedCount, err := getCount("building")
	if err != nil {
		t.Errorf("Error getting updated building count: %v", err)
		return
	}

	// Check if the count has decreased by 1
	if updatedCount != initialCount-1 {
		t.Errorf("Expected building count to decrease by 1, but got %d (initial: %d, updated: %d)", initialCount-updatedCount, initialCount, updatedCount)
	}
}

func TestInsertDepartment(t *testing.T) {
	OpenDB()
	buildingID := 5
	departmentName := "NewDepartment"

	// Get initial count of departments
	initialCount, err := getCount("department")
	if err != nil {
		t.Errorf("Error getting initial department count: %v", err)
		return
	}

	// Insert a new department
	err = other.InsertDepartment(buildingID, departmentName)
	if err != nil {
		t.Errorf("Error inserting department: %v", err)
		return
	}

	// Get updated count of departments
	updatedCount, err := getCount("department")
	if err != nil {
		t.Errorf("Error getting updated department count: %v", err)
		return
	}

	// Check if the count has increased by 1
	if updatedCount != initialCount+1 {
		t.Errorf("Expected department count to increase by 1, but got %d (initial: %d, updated: %d)", updatedCount-initialCount, initialCount, updatedCount)
	}
}
func TestEditDepartment(t *testing.T) {
	OpenDB()
	departmentID := 10
	newDepartmentName := "UpdatedDepartment"

	// Get initial department name
	var initialName string
	err := other.DB.QueryRow("SELECT name FROM department WHERE id=?", departmentID).Scan(&initialName)
	if err != nil {
		t.Errorf("Error getting initial department name: %v", err)
		return
	}

	// Edit the department name
	err = other.EditDepartment(departmentID, newDepartmentName)
	if err != nil {
		t.Errorf("Error editing department: %v", err)
		return
	}

	// Get updated department name
	var updatedName string
	err = other.DB.QueryRow("SELECT name FROM department WHERE id=?", departmentID).Scan(&updatedName)
	if err != nil {
		t.Errorf("Error getting updated department name: %v", err)
		return
	}

	// Check if the name has been updated
	if updatedName != newDepartmentName {
		t.Errorf("Expected department name to be %s, but got %s", newDepartmentName, updatedName)
	}
}
func TestDeleteDepartment(t *testing.T) {
	OpenDB()
	departmentID := 10

	// Get initial count of departments
	initialCount, err := getCount("department")
	if err != nil {
		t.Errorf("Error getting initial department count: %v", err)
		return
	}

	// Delete a department
	err = other.DeleteDepartment(departmentID)
	if err != nil {
		t.Errorf("Error deleting department: %v", err)
		return
	}

	// Get updated count of departments
	updatedCount, err := getCount("department")
	if err != nil {
		t.Errorf("Error getting updated department count: %v", err)
		return
	}

	// Check if the count has decreased by 1
	if updatedCount != initialCount-1 {
		t.Errorf("Expected department count to decrease by 1, but got %d (initial: %d, updated: %d)", initialCount-updatedCount, initialCount, updatedCount)
	}
}

func TestInsertEnoek(t *testing.T) {

	OpenDB()

	token := "0321412"
	header := "TestHeader"
	description := "TestDescription"
	startDate := "2024-02-20"
	stopDate := "2024-02-25"
	processID := 1

	var exists bool
	err := other.DB.QueryRow(`
	SELECT EXISTS(
		SELECT 1
		FROM enoek_suggestion_measures
		WHERE header = ?
		  AND description = ?
		  AND start_date = ?
		  AND stop_date = ?
		  AND process_id = ?
	)`, header, description, startDate, stopDate, processID).Scan(&exists)

	if exists {
		_, err = other.DB.Exec(`
	 DELETE FROM enoek_suggestion_measures
	  WHERE header = ?
	  AND description = ?
	  AND start_date = ?
	  AND stop_date = ?
	  AND process_id = ?`,
			header, description, startDate, stopDate, processID)
		if err != nil {
			t.Errorf("Error: %v", err)
			return
		}
	}
	err = other.InsertEnoek(token, header, description, startDate, stopDate, processID)
	if err != nil {
		t.Errorf("Error: %v", err)
		return
	}
	var insertedData other.EnokSuggestionMeasures
	err = other.DB.QueryRow(`
	SELECT header, description, author, start_date, stop_date, process_id
	FROM enoek_suggestion_measures
	WHERE header = ?
	  AND description = ?
	  AND start_date = ?
	  AND stop_date = ?
	  AND process_id = ?`,
		header, description, startDate, stopDate, processID).Scan(
		&insertedData.Header,
		&insertedData.Description,
		&insertedData.Author,
		&insertedData.StartDate,
		&insertedData.StopDate,
		&insertedData.ProcessID,
	)
	if err != nil {
		t.Errorf("Error: %v", err)
		return
	}
	if insertedData.Header != header ||
		insertedData.Description != description ||
		insertedData.Author != 1 ||
		insertedData.StartDate != startDate ||
		insertedData.StopDate != stopDate ||
		insertedData.ProcessID != processID {
		// Data doesn't match, handle the mismatch
		t.Errorf("Inserted data doesn't match expected values")
	}
}
func TestEnoekJugde(t *testing.T) {
	OpenDB()

	header := "TestHeader"
	description := "TestDescription"
	startDate := "2024-02-20"
	stopDate := "2024-02-25"
	processID := 1
	active := true
	approved := true

	id := 5

	err := other.EnoekJudgement(id, approved)
	if err != nil {
		t.Errorf("Error: %v", err)
		return
	}
	var insertedData other.EnokSuggestionMeasures
	err = other.DB.QueryRow(`
	SELECT header, description, author, start_date, stop_date, active, process_id, approved
	FROM enoek_suggestion_measures
	WHERE id = ?`,
		id).Scan(
		&insertedData.Header,
		&insertedData.Description,
		&insertedData.Author,
		&insertedData.StartDate,
		&insertedData.StopDate,
		&insertedData.Active,
		&insertedData.ProcessID,
		&insertedData.Approved,
	)
	if err != nil {
		t.Errorf("Error: %v", err)
		return
	}
	if insertedData.Header != header ||
		insertedData.Description != description ||
		insertedData.Author != 1 ||
		insertedData.StartDate != startDate ||
		insertedData.StopDate != stopDate ||
		insertedData.Active != active ||
		insertedData.ProcessID != processID ||
		insertedData.Approved != approved {
		// Data doesn't match, handle the mismatch
		t.Errorf("Inserted data doesn't match expected values")
	}
}
func TestEnoekActive(t *testing.T) {
	OpenDB()

	header := "TestHeader"
	description := "TestDescription"
	startDate := "2024-02-20"
	stopDate := "2024-02-25"
	processID := 1
	active := true

	id := 5

	err := other.EnoekActiveOrNot(id, active)
	if err != nil {
		t.Errorf("Error: %v", err)
		return
	}
	var insertedData other.EnokSuggestionMeasures
	err = other.DB.QueryRow(`
	SELECT header, description, author, start_date, stop_date, active, process_id
	FROM enoek_suggestion_measures
	WHERE id = ?`,
		id).Scan(
		&insertedData.Header,
		&insertedData.Description,
		&insertedData.Author,
		&insertedData.StartDate,
		&insertedData.StopDate,
		&insertedData.Active,
		&insertedData.ProcessID,
	)
	if err != nil {
		t.Errorf("Error: %v", err)
		return
	}
	if insertedData.Header != header ||
		insertedData.Description != description ||
		insertedData.Author != 1 ||
		insertedData.StartDate != startDate ||
		insertedData.StopDate != stopDate ||
		insertedData.Active != active ||
		insertedData.ProcessID != processID {
		// Data doesn't match, handle the mismatch
		t.Errorf("Inserted data doesn't match expected values")
	}
}
func TestDeleteEnoek(t *testing.T) {
	OpenDB()
	id := 5

	// Get initial count of users
	initialCount, err := getCount("enoek_suggestion_measures")
	if err != nil {
		t.Errorf("Error getting initial user count: %v", err)
		return
	}

	// Delete Enoek
	err = other.DeleteEnoek(id)
	if err != nil {
		t.Errorf("Error deleting user: %v", err)
		return
	}

	// Get updated count of users
	updatedCount, err := getCount("enoek_suggestion_measures")
	if err != nil {
		t.Errorf("Error getting updated user count: %v", err)
		return
	}

	// Check if the count has decreased by 1
	if updatedCount != initialCount-1 {
		t.Errorf("Expected user count to decrease by 1, but got %d (initial: %d, updated: %d)", initialCount-updatedCount, initialCount, updatedCount)
	}
}

func TestInsertCompany(t *testing.T) {
	OpenDB()
	companyName := "TestCompany"

	// Get initial count of companies
	initialCount, err := getCount("company")
	if err != nil {
		t.Errorf("Error getting initial company count: %v", err)
		return
	}

	// Insert a new company
	err = other.InsertCompany(companyName)
	if err != nil {
		t.Errorf("Error inserting company: %v", err)
		return
	}

	// Get updated count of companies
	updatedCount, err := getCount("company")
	if err != nil {
		t.Errorf("Error getting updated company count: %v", err)
		return
	}

	// Check if the count has increased by 1
	if updatedCount != initialCount+1 {
		t.Errorf("Expected company count to increase by 1, but got %d (initial: %d, updated: %d)", updatedCount-initialCount, initialCount, updatedCount)
	}
}
func TestEditCompany(t *testing.T) {
	OpenDB()
	companyName := "EditedCompany"
	id := 4

	// Edit the company name
	err := other.EditCompany(id, companyName)
	if err != nil {
		t.Errorf("Error editing company: %v", err)
		return
	}

	// Check if the company name has been updated
	var updatedName string
	err = other.DB.QueryRow(`
		SELECT name
		FROM company
		WHERE id = ?`, id).Scan(&updatedName)
	if err != nil {
		t.Errorf("Error getting updated company name: %v", err)
		return
	}

	// Check if the name has been updated
	if updatedName != companyName {
		t.Errorf("Expected company name to be %s, but got %s", companyName, updatedName)
	}
}

func TestInsertSensor(t *testing.T) {
	OpenDB()

	machineDetails := other.Machine{
		EUI:          "newEUI",
		Name:         "Furnace",
		ExpectedUse:  10,
		MachineNr:    "A573",
		BuildingID:   1,
		DepartmentID: 0,
	}
	processID := 1

	var exists bool
	err := other.DB.QueryRow(`
	SELECT EXISTS(
		SELECT 1
		FROM machine
		WHERE eui = ?
	)`, machineDetails.EUI).Scan(&exists)

	if exists {
		_, err = other.DB.Exec(`
	 DELETE FROM machine_processes
	  WHERE machine_id = ?
	  AND processes_id = ?`,
			machineDetails.EUI, processID)
		if err != nil {
			t.Errorf("Error: %v", err)
			return
		}
		_, err = other.DB.Exec(`
	 DELETE FROM machine
	  WHERE eui = ?`,
			machineDetails.EUI)
		if err != nil {
			t.Errorf("Error: %v", err)
			return
		}
	}

	err = other.InsertSensor(machineDetails, processID)
	if err != nil {
		t.Errorf("Error: %v", err)
		return
	}
	var insertedData other.Machine
	err = other.DB.QueryRow(`
	SELECT *
	FROM machine
	WHERE eui = ?`,
		machineDetails.EUI).Scan(
		&insertedData.EUI,
		&insertedData.Name,
		&insertedData.ExpectedUse,
		&insertedData.MachineNr,
		&insertedData.BuildingID,
		&insertedData.DepartmentID,
	)
	if err != nil {
		t.Errorf("Error: %v", err)
		return
	}
	var insertedProcess other.MachineProcesses
	err = other.DB.QueryRow(`
	SELECT processes_id
	FROM machine_processes
	WHERE machine_id = ?`,
		machineDetails.EUI).Scan(
		&insertedProcess.ProcessesID,
	)
	if err != nil {
		t.Errorf("Error: %v", err)
		return
	}
	if insertedData.EUI != machineDetails.EUI ||
		insertedData.Name != machineDetails.Name ||
		insertedData.ExpectedUse != machineDetails.ExpectedUse ||
		insertedData.MachineNr != machineDetails.MachineNr ||
		insertedData.BuildingID != machineDetails.BuildingID ||
		insertedData.DepartmentID != machineDetails.DepartmentID ||
		insertedProcess.ProcessesID != processID {
		// Data doesn't match, handle the mismatch
		t.Errorf("Inserted data doesn't match expected values")
	}
}
func TestEditSensor(t *testing.T) {
	OpenDB()

	machineDetails := other.Machine{
		EUI:          "newEUI",
		Name:         "Furnace",
		ExpectedUse:  10,
		MachineNr:    "A573",
		BuildingID:   1,
		DepartmentID: 1,
	}
	newMachineDetails := other.Machine{
		EUI:          "newEUI",
		Name:         "Furnace",
		ExpectedUse:  12,
		MachineNr:    "A577",
		BuildingID:   1,
		DepartmentID: 2,
	}

	// Edit the sensor
	err := other.EditSensor(newMachineDetails)
	if err != nil {
		t.Errorf("Error editing sensor: %v", err)
		return
	}

	var updatedData other.Machine
	err = other.DB.QueryRow(`
	SELECT *
	FROM machine
	WHERE eui = ?`,
		machineDetails.EUI).Scan(
		&updatedData.EUI,
		&updatedData.Name,
		&updatedData.ExpectedUse,
		&updatedData.MachineNr,
		&updatedData.BuildingID,
		&updatedData.DepartmentID,
	)
	if err != nil {
		t.Errorf("Error: %v", err)
		return
	}

	// Check if the data has been updated
	if updatedData.EUI != newMachineDetails.EUI ||
		updatedData.Name != newMachineDetails.Name ||
		updatedData.ExpectedUse != newMachineDetails.ExpectedUse ||
		updatedData.MachineNr != newMachineDetails.MachineNr ||
		updatedData.BuildingID != newMachineDetails.BuildingID ||
		updatedData.DepartmentID != newMachineDetails.DepartmentID {
		// Data doesn't match, handle the mismatch
		t.Errorf("Inserted data doesn't match expected values")
	}
}
func TestDeleteSensor(t *testing.T) {
	OpenDB()

	EUI := "newEUI"

	// Get initial count of sensors
	initialCount, err := getCount("machine")
	if err != nil {
		t.Errorf("Error getting initial sensor count: %v", err)
		return
	}

	// Delete the sensor
	err = other.DeleteSensor(EUI)
	if err != nil {
		t.Errorf("Error deleting sensor: %v", err)
		return
	}

	// Get updated count of sensors
	updatedCount, err := getCount("machine")
	if err != nil {
		t.Errorf("Error getting updated sensor count: %v", err)
		return
	}

	// Check if the count has decreased by 1
	if updatedCount != initialCount-1 {
		t.Errorf("Expected sensor count to decrease by 1, but got %d (initial: %d, updated: %d)", initialCount-updatedCount, initialCount, updatedCount)
	}
}

func TestInsertSensorData(t *testing.T) {
	OpenDB()
	sensorDataExp := other.SensorData{
		EUI:          "E890E1CC8CF44B49",
		Accumulation: 10.5,
		AVGCurrent:   2.5,
		OffsetMax:    1.0,
		OffsetMin:    0.5,
		Voltage:      220.0,
		Temperature:  25.0,
		//DateTime:     "2024-02-28 12:00:00",
	}

	err := other.InsertSensorData(sensorDataExp)
	if err != nil {
		t.Errorf("Error: %v", err)
		return
	}
	var insertedData other.SensorData
	err = other.DB.QueryRow(`
	SELECT *
	FROM sensorData
	WHERE eui = ?`,
		sensorDataExp.EUI).Scan(
		&insertedData.EUI,
		&insertedData.Accumulation,
		&insertedData.AVGCurrent,
		&insertedData.OffsetMax,
		&insertedData.OffsetMin,
		&insertedData.Voltage,
		&insertedData.Temperature,
		&insertedData.DateTime,
	)
	if err != nil {
		t.Errorf("Error: %v", err)
		return
	}
	if insertedData.EUI != sensorDataExp.EUI ||
		insertedData.Accumulation != sensorDataExp.Accumulation ||
		insertedData.AVGCurrent != sensorDataExp.AVGCurrent ||
		insertedData.OffsetMax != sensorDataExp.OffsetMax ||
		insertedData.OffsetMin != sensorDataExp.OffsetMin ||
		insertedData.Voltage != sensorDataExp.Voltage ||
		insertedData.Temperature != sensorDataExp.Temperature ||
		insertedData.DateTime != sensorDataExp.DateTime {
		// Data doesn't match, handle the mismatch
		t.Errorf("Inserted data doesn't match expected values")
	}
}
func TestClearSensorData(t *testing.T) {
	OpenDB()

	// Clear sensor data
	err := other.ClearSensorData()
	if err != nil {
		t.Errorf("Error clearing sensor data: %v", err)
		return
	}

	// Get updated count of sensor data
	updatedCount, err := getCount("sensorData")
	if err != nil {
		t.Errorf("Error getting updated sensor data count: %v", err)
		return
	}

	// Check if the count has decreased to 0
	if updatedCount != 0 {
		t.Errorf("Expected sensor data count to be 0 after clearing, but got %d", updatedCount)
	}
}

func TestInsertProcess(t *testing.T) {
	OpenDB()

	token := "0321412"
	processDetails := other.Processes{
		Name:        "newProcess",
		Description: "Test test test test test",
	}
	userID, err := other.GetUserIDFromToken(token)
	compID, err := other.GetCompanyFromUserID(userID)

	var exists bool
	err = other.DB.QueryRow(`
	SELECT EXISTS(
		SELECT 1
		FROM processes
		WHERE name = ?
		  AND description = ?
	)`, processDetails.Name, processDetails.Description).Scan(&exists)

	if exists {
		_, err = other.DB.Exec(`
	 DELETE FROM processes
	  WHERE name = ?
		  AND description = ?`,
			processDetails.Name, processDetails.Description)
		if err != nil {
			t.Errorf("Error: %v", err)
			return
		}
	}
	err = other.InsertProcess(token, processDetails)
	if err != nil {
		t.Errorf("Error: %v", err)
		return
	}
	var insertedData other.Processes
	err = other.DB.QueryRow(`
	SELECT name, description, company_id
	FROM processes
	WHERE name = ?
		  AND description = ?`,
		processDetails.Name, processDetails.Description).Scan(
		&insertedData.Name,
		&insertedData.Description,
		&insertedData.CompanyID,
	)
	if err != nil {
		t.Errorf("Error: %v", err)
		return
	}
	if insertedData.Name != processDetails.Name ||
		insertedData.Description != processDetails.Description ||
		insertedData.CompanyID != compID {
		// Data doesn't match, handle the mismatch
		t.Errorf("Inserted data doesn't match expected values")
	}
}
func TestEditProcess(t *testing.T) {
	OpenDB()

	processDetails := other.Processes{
		ID:          3,
		Name:        "newProcessVer2",
		Description: "Test test test test test test",
	}

	// Edit the process
	err := other.EditProcess(processDetails)
	if err != nil {
		t.Errorf("Error editing process: %v", err)
		return
	}

	// Check if the process has been updated
	var updatedData other.Processes
	err = other.DB.QueryRow(`
	SELECT name, description
	FROM processes
	WHERE id = ?`,
		processDetails.ID).Scan(
		&updatedData.Name,
		&updatedData.Description,
	)
	if err != nil {
		t.Errorf("Error: %v", err)
		return
	}

	// Check if the data has been updated
	if updatedData.Name != processDetails.Name ||
		updatedData.Description != processDetails.Description {
		t.Errorf("Updated data doesn't match expected values")
	}
}
func TestDeleteProcess(t *testing.T) {
	OpenDB()

	id := 3

	// Get initial count of processes
	initialCount, err := getCount("processes")
	if err != nil {
		t.Errorf("Error getting initial process count: %v", err)
		return
	}

	// Delete the process
	err = other.DeleteProcess(id)
	if err != nil {
		t.Errorf("Error deleting process: %v", err)
		return
	}

	// Get updated count of processes
	updatedCount, err := getCount("processes")
	if err != nil {
		t.Errorf("Error getting updated process count: %v", err)
		return
	}

	// Check if the count has decreased by 1
	if updatedCount != initialCount-1 {
		t.Errorf("Expected process count to decrease by 1, but got %d (initial: %d, updated: %d)", initialCount-updatedCount, initialCount, updatedCount)
	}
}

func TestInsertProcessMachine(t *testing.T) {
	OpenDB()

	data := other.MachineProcesses{
		MachineID:   "newEUI",
		ProcessesID: 4,
	}

	var exists bool
	err := other.DB.QueryRow(`
	SELECT EXISTS(
		SELECT 1
		FROM machine_processes
		WHERE machine_id = ?
		AND processes_id = ?
	)`, data.MachineID, data.ProcessesID).Scan(&exists)

	if exists {
		_, err = other.DB.Exec(`
	 DELETE FROM machine_processes
		WHERE machine_id = ?
		AND processes_id = ?`,
			data.MachineID, data.ProcessesID)
		if err != nil {
			t.Errorf("Error: %v", err)
			return
		}
	}
	err = other.AddSensorToProcess(data)
	if err != nil {
		t.Errorf("Error: %v", err)
		return
	}
	var insertedData other.MachineProcesses
	err = other.DB.QueryRow(`
	SELECT *
	FROM machine_processes
		WHERE machine_id = ?
		AND processes_id = ?`,
		data.MachineID, data.ProcessesID).Scan(
		&insertedData.MachineID,
		&insertedData.ProcessesID,
	)
	if err != nil {
		t.Errorf("Error: %v", err)
		return
	}
	if insertedData.MachineID != data.MachineID ||
		insertedData.ProcessesID != data.ProcessesID {
		// Data doesn't match, handle the mismatch
		t.Errorf("Inserted data doesn't match expected values")
	}
}

func TestDeleteProcessMachine(t *testing.T) {
	OpenDB()

	data := other.MachineProcesses{
		MachineID:   "newEUI",
		ProcessesID: 4,
	}

	// Get initial count of process machines
	initialCount, err := getCount("machine_processes")
	if err != nil {
		t.Errorf("Error getting initial process machine count: %v", err)
		return
	}

	// Delete the process machine
	err = other.DeleteSensorToProcess(data)
	if err != nil {
		t.Errorf("Error deleting process machine: %v", err)
		return
	}

	// Get updated count of process machines
	updatedCount, err := getCount("machine_processes")
	if err != nil {
		t.Errorf("Error getting updated process machine count: %v", err)
		return
	}

	// Check if the count has decreased by 1
	if updatedCount != initialCount-1 {
		t.Errorf("Expected process machine count to decrease by 1, but got %d (initial: %d, updated: %d)", initialCount-updatedCount, initialCount, updatedCount)
	}
}

func TestInsertGateway(t *testing.T) {
	OpenDB()

	compId := 1
	eui := "142ee803b99e06d2"
	token := "0321412"

	var exists bool
	err := other.DB.QueryRow(`
	SELECT EXISTS(
		SELECT 1
		FROM gateway
		WHERE EUI = ?
		  AND company_id = ?
	)`, eui, compId).Scan(&exists)

	if exists {
		_, err = other.DB.Exec(`
	 DELETE FROM gateway
	  WHERE EUI = ?
	    AND company_id = ?`,
			eui, compId)
		if err != nil {
			t.Errorf("Error: %v", err)
			return
		}
	}
	err = other.AddGateway(token, eui)
	if err != nil {
		t.Errorf("Error: %v", err)
		return
	}
	var insertedData other.Gateway
	err = other.DB.QueryRow(`
	SELECT *
	FROM gateway
	WHERE eui = ?
	    AND company_id = ?`,
		eui, compId).Scan(
		&insertedData.EUI,
		&insertedData.CompanyID,
	)
	if err != nil {
		t.Errorf("Error: %v", err)
		return
	}
	if insertedData.EUI != eui ||
		insertedData.CompanyID != compId {
		// Data doesn't match, handle the mismatch
		t.Errorf("Inserted data doesn't match expected values")
	}
}
func TestEditGateway(t *testing.T) {
	OpenDB()

	// Initial data
	oldEui := "142ee803b99e06d2"
	compId := 1
	newEui := "142ee803b99e06d3"

	// Edit the gateway
	err := other.EditGateway(newEui, oldEui)
	if err != nil {
		t.Errorf("Error editing gateway: %v", err)
		return
	}

	// Check if the gateway has been updated
	var updatedData other.Gateway
	err = other.DB.QueryRow(`
	SELECT *
	FROM gateway
	WHERE eui = ?
	    AND company_id = ?`,
		newEui, compId).Scan(
		&updatedData.EUI,
		&updatedData.CompanyID,
	)
	if err != nil {
		t.Errorf("Error: %v", err)
		return
	}

	// Check if the data has been updated
	if updatedData.EUI != newEui ||
		updatedData.CompanyID != compId {
		t.Errorf("Updated data doesn't match expected values")
	}
}
func TestDeleteGateway(t *testing.T) {
	OpenDB()

	// Initial data
	euiToDelete := "142ee803b99e06d3"

	// Get initial count of gateways
	initialCount, err := getCount("gateway")
	if err != nil {
		t.Errorf("Error getting initial gateway count: %v", err)
		return
	}

	// Delete the gateway
	err = other.DeleteGateway(euiToDelete)
	if err != nil {
		t.Errorf("Error deleting gateway: %v", err)
		return
	}

	// Get updated count of gateways
	updatedCount, err := getCount("gateway")
	if err != nil {
		t.Errorf("Error getting updated gateway count: %v", err)
		return
	}

	// Check if the count has decreased by 1
	if updatedCount != initialCount-1 {
		t.Errorf("Expected gateway count to decrease by 1, but got %d (initial: %d, updated: %d)", initialCount-updatedCount, initialCount, updatedCount)
	}
}
