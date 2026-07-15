package other

import (
	"crypto/rand"
	"database/sql"
	_ "database/sql"
	"encoding/base64"
	"io"
	"log"
	"time"
)

func getUserIDFromEmail(email string) (int, error) {
	var userId int
	err := DB.QueryRow("SELECT id FROM users WHERE email = ?", email).Scan(&userId)
	if err != nil {
		return 0, err
	}
	return userId, nil
}

func GetHashedPasswordFromEmail(email string) ([]byte, error) {
	var hashString string
	id, err := getUserIDFromEmail(email) //get from token
	if err != nil {
		return nil, err
	}
	err = DB.QueryRow("SELECT password FROM users WHERE id = ?", id).Scan(&hashString)
	if err != nil {
		return nil, err
	}
	hash := []byte(hashString)

	return hash, nil
}

func getUserIDFromToken(token string) (int, error) {
	var userID int
	// Query to retrieve user ID based on session token
	err := DB.QueryRow("SELECT user_id FROM session WHERE token = ?", token).Scan(&userID)
	if err != nil {
		return 0, err
	}
	return userID, nil
}
func GetUserIDFromEmail(email string) (int, error) {
	var userID int
	// Query to retrieve user ID based on email
	err := DB.QueryRow("SELECT id FROM users WHERE email = ?", email).Scan(&userID)
	if err != nil {
		return 0, err
	}
	return userID, nil
}

func getCompanyFromUserID(userID int) (int, error) {
	var companyID int
	// Query to retrieve company ID based on user ID
	err := DB.QueryRow("SELECT company_id FROM users WHERE id = ?", userID).Scan(&companyID)
	if err != nil {
		return 0, err
	}
	return companyID, nil
}

func GetPermissionFromToken(token string) (int, error) {
	var permission int
	// Query to retrieve user ID based on session token
	err := DB.QueryRow("SELECT u.permission FROM session s JOIN users u ON s.user_id = u.id WHERE s.token = ?", token).Scan(&permission)
	if err != nil {
		return 0, err
	}
	return permission, nil
}

// SeeUsers Gives data about all users, is for userconfig for admin
func SeeUsers(token string) ([]Users, error) {
	//gets userID
	userID, err := getUserIDFromToken(token)
	if err != nil {
		return nil, err
	}
	//gets the company id from user ID
	compID, err := getCompanyFromUserID(userID)
	if err != nil {
		return nil, err
	}
	//query to give all user data from a company that admin is connected to
	rows, err := DB.Query(`
		SELECT u.id, u.email, u.first_name, u.last_name, u.permission
		FROM users u
		WHERE u.company_id = ?`, compID)
	if err != nil {
		return nil, err
	}
	defer func(rows *sql.Rows) {
		err := rows.Close()
		if err != nil {

		}
	}(rows)

	var userInfos []Users
	//stores the data
	for rows.Next() {
		var userInfo Users
		if err := rows.Scan(
			&userInfo.ID,
			&userInfo.Email,
			&userInfo.FirstName,
			&userInfo.LastName,
			&userInfo.Permission,
		); err != nil {
			return nil, err
		}
		userInfos = append(userInfos, userInfo)
	}

	if err := rows.Err(); err != nil {
		return nil, err
	}

	return userInfos, nil
}

// GetSensors Is giving information about the machine/sensor tab to be able to get an overview or existing sensors
func GetSensors(token string) ([]SensorInfo, error) {

	userID, err := getUserIDFromToken(token)
	if err != nil {
		return nil, err
	}
	//gets the company id from user ID
	compID, err := getCompanyFromUserID(userID)
	if err != nil {
		return nil, err
	}
	rows, err := DB.Query(`
		SELECT
			m.eui,
			m.name AS machine_name,
			m.expected_use,
			m.machineNr,
			m.voltage,
			b.name AS building_name,
			d.name AS department_name
		FROM
			machine m
		JOIN
			building b ON m.building_id = b.id
		LEFT JOIN
			department d ON m.department_id = d.id
		WHERE
			b.company_id = ?`, compID)
	if err != nil {
		return nil, err
	}
	defer rows.Close()

	var machineInfos []SensorInfo
	var tempVoltage sql.NullInt64

	for rows.Next() {
		var machineInfo SensorInfo

		if err := rows.Scan(
			&machineInfo.EUI,
			&machineInfo.MachineName,
			&machineInfo.ExpectedUse,
			&machineInfo.MachineNr,
			&tempVoltage,
			&machineInfo.BuildingName,
			&machineInfo.DepartmentName,
		); err != nil {
			return nil, err
		}
		if tempVoltage.Valid {
			machineInfo.Voltage = int(tempVoltage.Int64)
		} else {
			machineInfo.Voltage = 230
		}
		machineInfos = append(machineInfos, machineInfo)
	}

	if err := rows.Err(); err != nil {
		return nil, err
	}

	return machineInfos, nil
}

// GetEnokSuggestions Is sending all enoek data for enoek page that is suggestions added
func GetEnokSuggestions(token string) ([]EnokSuggestionMeasures, error) {
	//gets userID
	userID, err := getUserIDFromToken(token)
	if err != nil {
		return nil, err
	}
	//gets the company id from user ID
	compID, err := getCompanyFromUserID(userID)
	if err != nil {
		return nil, err
	}

	// Query to fetch enøk suggestions
	rows, err := DB.Query(`
		SELECT
			esm.header,
			esm.description,
			esm.author,
			esm.start_date,
			esm.stop_date,
			esm.process_id
		FROM
			enoek_suggestion_measures esm
		JOIN
			users u ON esm.author = u.id
		WHERE
			esm.active = false
			AND u.company_id = ?`, compID)
	if err != nil {
		return nil, err
	}
	defer rows.Close()

	var enokSuggestions []EnokSuggestionMeasures

	for rows.Next() {
		var suggestion EnokSuggestionMeasures
		if err := rows.Scan(
			&suggestion.Header,
			&suggestion.Description,
			&suggestion.Author,
			&suggestion.StartDate,
			&suggestion.StopDate,
			&suggestion.ProcessID,
		); err != nil {
			return nil, err
		}
		enokSuggestions = append(enokSuggestions, suggestion)
	}

	if err := rows.Err(); err != nil {
		return nil, err
	}

	return enokSuggestions, nil
}

// GetEnokMeasures Is sending all enoek data that is active
func GetEnokMeasures(token string) ([]EnokSuggestionMeasures, error) {
	//gets userID
	userID, err := getUserIDFromToken(token)
	if err != nil {
		return nil, err
	}
	//gets the company id from user ID
	compID, err := getCompanyFromUserID(userID)
	if err != nil {
		return nil, err
	}

	// Query to fetch enøk suggestions
	rows, err := DB.Query(`
		SELECT
			esm.header,
			esm.description,
			esm.author,
			esm.start_date,
			esm.stop_date,
			esm.process_id
		FROM
			enoek_suggestion_measures esm
		JOIN
			users u ON esm.author = u.id
		WHERE
			esm.active = true
			AND u.company_id = ?`, compID)
	if err != nil {
		return nil, err
	}
	defer rows.Close()

	var enokSuggestions []EnokSuggestionMeasures

	for rows.Next() {
		var suggestion EnokSuggestionMeasures
		if err := rows.Scan(
			&suggestion.Header,
			&suggestion.Description,
			&suggestion.Author,
			&suggestion.StartDate,
			&suggestion.StopDate,
			&suggestion.ProcessID,
		); err != nil {
			return nil, err
		}
		enokSuggestions = append(enokSuggestions, suggestion)
	}

	if err := rows.Err(); err != nil {
		return nil, err
	}

	return enokSuggestions, nil
}

// GetAllEnoek simple function to get all enoek data with this
func GetAllEnoek(token string) ([]EnokSuggestionMeasures, error) {
	//gets userID
	userID, err := getUserIDFromToken(token)
	if err != nil {
		return nil, err
	}
	//gets the company id from user ID
	compID, err := getCompanyFromUserID(userID)
	if err != nil {
		return nil, err
	}

	// Query to fetch enøk suggestions
	rows, err := DB.Query(`
		SELECT
			esm.*
		FROM
			enoek_suggestion_measures esm
		JOIN
			users u ON esm.author = u.id
		WHERE
			 u.company_id = ?`, compID)
	if err != nil {
		return nil, err
	}
	defer rows.Close()

	var enokSuggestions []EnokSuggestionMeasures

	for rows.Next() {
		var suggestion EnokSuggestionMeasures
		if err := rows.Scan(
			&suggestion.ID,
			&suggestion.Header,
			&suggestion.Description,
			&suggestion.Author,
			&suggestion.StartDate,
			&suggestion.StopDate,
			&suggestion.Active,
			&suggestion.Approved,
			&suggestion.ProcessID,
		); err != nil {
			return nil, err
		}
		enokSuggestions = append(enokSuggestions, suggestion)
	}

	if err := rows.Err(); err != nil {
		return nil, err
	}

	return enokSuggestions, nil
}

// GetMachineDetailsForUser gets sesnor data based upon which company the user that is viewing is working for
func GetMachineDetailsForUser(token string) ([]MachineDetails, error) {
	//gets userID
	userID, err := getUserIDFromToken(token)
	if err != nil {
		return nil, err
	}
	//gets the company id from user ID
	compID, err := getCompanyFromUserID(userID)
	if err != nil {
		return nil, err
	}

	rows, err := DB.Query(`
		SELECT
			m.*,
			b.*,
			d.*,
			sd.*,
			p.*
		FROM
			machine m
		JOIN
			building b ON m.building_id = b.id
		LEFT JOIN
			department d ON m.department_id = d.id
		INNER JOIN 
			sensorData sd ON m.eui = sd.eui
		LEFT JOIN
			machine_processes mp ON m.eui = mp.machine_id
		LEFT JOIN
			processes p ON mp.processes_id = p.id
		WHERE
			b.company_id = ?`, compID)
	if err != nil {
		return nil, err
	}
	defer rows.Close()

	var machineDetailsMap = make(map[string]MachineDetails)
	i := 0
	for rows.Next() {
		log.Println(i)
		i++
		var machineDetails MachineDetails
		var machine Machine
		var building Building
		var department Department
		var sensorData SensorData
		var process Processes
		//Nullable variables for nullable values such as department and process
		var processId sql.NullInt64
		var processName sql.NullString
		var processDesc sql.NullString
		var processCID sql.NullInt64

		var departmentId sql.NullInt64
		var departmentBuildingId sql.NullInt64
		var departmentName sql.NullString

		var tempVoltage sql.NullInt64

		if err := rows.Scan(
			&machine.EUI,
			&machine.Name,
			&machine.ExpectedUse,
			&machine.MachineNr,
			&machine.BuildingID,
			&departmentId,
			&tempVoltage,
			&building.ID,
			&building.CompanyID,
			&building.Name,
			&departmentId,
			&departmentBuildingId,
			&departmentName,
			&sensorData.EUI,
			&sensorData.Acumulation,
			&sensorData.AVGCurrent,
			&sensorData.OffsetMax,
			&sensorData.OffsetMin,
			&sensorData.Voltage,
			&sensorData.Temperature,
			&sensorData.DateTime,
			&processId,
			&processName,
			&processDesc,
			&processCID,
		); err != nil {
			return nil, err
		}
		if processId.Valid {
			process.ID = int(processId.Int64)
			process.Name = processName.String
			process.Description = processDesc.String
			process.CompanyID = int(processCID.Int64)
		}
		if departmentId.Valid {
			machine.DepartmentID = int(departmentId.Int64)
			department.ID = int(departmentId.Int64)
			department.Name = departmentName.String
			department.BuildingID = int(departmentBuildingId.Int64)
		}
		if tempVoltage.Valid {
			machine.Voltage = int(tempVoltage.Int64)
		} else {
			machine.Voltage = 230
		}
		machineDetails.Machine = machine
		machineDetails.Building = building
		machineDetails.Department = department

		existingDetails, exists := machineDetailsMap[machineDetails.Machine.EUI]
		if exists {
			// Check if SensorData entry already exists
			sensorDataExists := false
			for _, existingSensorData := range existingDetails.SensorDataList {
				if existingSensorData.EUI == sensorData.EUI && existingSensorData.DateTime == sensorData.DateTime {
					sensorDataExists = true
					break
				}
			}

			// If SensorData doesn't exist, append it
			if !sensorDataExists {
				existingDetails.SensorDataList = append(existingDetails.SensorDataList, sensorData)
			}

			// Check if Processes entry already exists
			processExists := false
			for _, existingProcess := range existingDetails.AssociatedProcesses {
				if existingProcess.ID == process.ID {
					processExists = true
					break
				}
			}

			// If Processes doesn't exist, append it
			if !processExists {
				existingDetails.AssociatedProcesses = append(existingDetails.AssociatedProcesses, process)
			}

			machineDetailsMap[machineDetails.Machine.EUI] = existingDetails
		} else {
			// Add new machineDetails to the map
			machineDetails.SensorDataList = append(machineDetails.SensorDataList, sensorData)
			machineDetails.AssociatedProcesses = append(machineDetails.AssociatedProcesses, process)
			machineDetailsMap[machineDetails.Machine.EUI] = machineDetails
		}
	}
	// Convert map values to a slice
	var machineDetailsList []MachineDetails
	for _, details := range machineDetailsMap {
		machineDetailsList = append(machineDetailsList, details)
		log.Println(details.Machine.Voltage)
	}
	if err := rows.Err(); err != nil {
		return nil, err
	}

	return machineDetailsList, nil
}

func GetMachinesForProcess(token string, process int) ([]MachineProcesses, []SimpleMachine, error) {
	//gets userID
	userID, err := getUserIDFromToken(token)
	if err != nil {
		return nil, nil, err
	}
	//gets the company id from user ID
	compID, err := getCompanyFromUserID(userID)
	if err != nil {
		return nil, nil, err
	}

	rows, err := DB.Query(`
		SELECT
			*
		FROM
			machine_processes
		WHERE
			processes_id = ?`, process)
	if err != nil {
		return nil, nil, err
	}
	defer rows.Close()

	var machinePros []MachineProcesses

	for rows.Next() {
		var machinePro MachineProcesses
		if err := rows.Scan(
			&machinePro.MachineID,
			&machinePro.ProcessesID,
		); err != nil {
			return nil, nil, err
		}
		machinePros = append(machinePros, machinePro)
	}

	if err := rows.Err(); err != nil {
		return nil, nil, err
	}
	rows2, err := DB.Query(`
		SELECT
			m.eui,
			m.name
		FROM
			machine m
		JOIN
			building b ON m.building_id = b.id
		WHERE
			b.company_id = ?`, compID)
	if err != nil {
		return nil, nil, err
	}
	defer rows2.Close()

	var machines []SimpleMachine

	for rows2.Next() {
		var machine SimpleMachine
		if err := rows2.Scan(
			&machine.EUI,
			&machine.Name,
		); err != nil {
			return nil, nil, err
		}
		machines = append(machines, machine)
	}

	if err := rows2.Err(); err != nil {
		return nil, nil, err
	}

	return machinePros, machines, nil
}

// GetBuildingsAndDepartments shows all buildings and departments inside the building
func GetBuildingsAndDepartments(token string) (map[Building][]Department, error) {
	buildings := make(map[Building][]Department)
	//gets userID
	userID, err := getUserIDFromToken(token)
	if err != nil {
		return nil, err
	}
	//gets the company id from user ID
	compID, err := getCompanyFromUserID(userID)
	if err != nil {
		return nil, err
	}
	// Query to fetch buildings and departments
	rows, err := DB.Query(`
		SELECT
			b.id AS building_id,
			b.name AS building_name,
			d.id AS department_id,
			d.name AS department_name
		FROM
			building b
		LEFT JOIN
			department d ON b.id = d.building_id
		WHERE
			b.company_id = ?`, compID)
	if err != nil {
		return nil, err
	}
	defer rows.Close()

	for rows.Next() {
		var building Building
		var department Department

		// Use pointers for fields that may be NULL
		var departmentID sql.NullInt64
		var departmentName sql.NullString

		err := rows.Scan(
			&building.ID,
			&building.Name,
			&departmentID,
			&departmentName,
		)
		if err != nil {
			return nil, err
		}

		// Check if departmentID is valid (not NULL)
		if departmentID.Valid {
			department.ID = int(departmentID.Int64)
		}
		// Check if departmentName is valid (not NULL)
		if departmentName.Valid {
			department.Name = departmentName.String
		}

		// Check if the building is already in the map
		if _, ok := buildings[building]; !ok {
			buildings[building] = make([]Department, 0)
		}

		// Add the department to the building's list
		buildings[building] = append(buildings[building], department)
	}

	if err := rows.Err(); err != nil {
		return nil, err
	}

	return buildings, nil
}

// GetProfile shows personal profile
func GetProfile(token string) ([]Profile, error) {
	//gets userID
	userID, err := getUserIDFromToken(token)
	if err != nil {
		return nil, err
	}
	rows, err := DB.Query(`
		SELECT 
		    id,
		    email,
		    first_name,
		    last_name,
		    permission
		FROM
			users
		WHERE
			id = ?`, userID)
	if err != nil {
		return nil, err
	}
	defer rows.Close()

	var userProfiles []Profile

	for rows.Next() {
		var userProfile Profile
		if err := rows.Scan(
			&userProfile.ID,
			&userProfile.Email,
			&userProfile.FirstName,
			&userProfile.LastName,
			&userProfile.Permission,
		); err != nil {
			return nil, err
		}
		userProfiles = append(userProfiles, userProfile)
	}

	if err := rows.Err(); err != nil {
		return nil, err
	}

	return userProfiles, nil
}

func GetPerson(id int) (string, error) {

	rows, err := DB.Query(`
		SELECT 
		    first_name,
		    last_name
		FROM
			users
		WHERE
			id = ?`, id)
	if err != nil {
		return "", err
	}
	defer rows.Close()

	var userProfiles []Profile

	for rows.Next() {
		var userProfile Profile
		if err := rows.Scan(
			&userProfile.FirstName,
			&userProfile.LastName,
		); err != nil {
			return "", err
		}
		userProfiles = append(userProfiles, userProfile)
	}
	var user = userProfiles[0].FirstName + " " + userProfiles[0].LastName

	if err := rows.Err(); err != nil {
		return "", err
	}

	return user, nil
}

// GetGateway shows gateway data
func GetGateway(token string) ([]Gateway, error) {
	//gets userID
	userID, err := getUserIDFromToken(token)
	if err != nil {
		return nil, err
	}
	//gets the company id from user ID
	compID, err := getCompanyFromUserID(userID)
	if err != nil {
		return nil, err
	}

	rows, err := DB.Query(`
		SELECT 
		    EUI,
		    name
		FROM
			gateway
		WHERE
			company_id = ?`, compID)
	if err != nil {
		return nil, err
	}
	defer rows.Close()

	var gatewayDatas []Gateway

	for rows.Next() {
		var gatewayData Gateway
		if err := rows.Scan(
			&gatewayData.EUI,
			&gatewayData.Name,
		); err != nil {
			return nil, err
		}
		gatewayDatas = append(gatewayDatas, gatewayData)
	}

	if err := rows.Err(); err != nil {
		return nil, err
	}

	return gatewayDatas, nil
}

func GetProcess(token string) ([]Processes, error) {
	//gets userID
	userID, err := getUserIDFromToken(token)
	if err != nil {
		return nil, err
	}
	//gets the company id from user ID
	compID, err := getCompanyFromUserID(userID)
	if err != nil {
		return nil, err
	}

	rows, err := DB.Query(`
		SELECT 
		    *
		FROM
			processes
		WHERE
			company_id = ?`, compID)
	if err != nil {
		return nil, err
	}
	defer rows.Close()

	var ProcessesInfo []Processes

	for rows.Next() {
		var ProcessInfo Processes
		if err := rows.Scan(
			&ProcessInfo.ID,
			&ProcessInfo.Name,
			&ProcessInfo.Description,
			&ProcessInfo.CompanyID,
		); err != nil {
			return nil, err
		}
		ProcessesInfo = append(ProcessesInfo, ProcessInfo)
	}

	if err := rows.Err(); err != nil {
		return nil, err
	}

	return ProcessesInfo, nil
}

func GetBuilding(token string) ([]Building, error) {
	//gets userID
	userID, err := getUserIDFromToken(token)
	if err != nil {
		return nil, err
	}
	//gets the company id from user ID
	compID, err := getCompanyFromUserID(userID)
	if err != nil {
		return nil, err
	}

	rows, err := DB.Query(`
		SELECT 
		    *
		FROM
			building
		WHERE
			company_id = ?`, compID)
	if err != nil {
		return nil, err
	}
	defer rows.Close()

	var BuildingInfos []Building

	for rows.Next() {
		var BuildingInfo Building
		if err := rows.Scan(
			&BuildingInfo.ID,
			&BuildingInfo.CompanyID,
			&BuildingInfo.Name,
		); err != nil {
			return nil, err
		}
		BuildingInfos = append(BuildingInfos, BuildingInfo)
	}

	if err := rows.Err(); err != nil {
		return nil, err
	}

	return BuildingInfos, nil
}

func MakeSessionToken(email string) (string, error) {
	tokenLength := 32

	var token string
	userId, err := getUserIDFromEmail(email)
	if err != nil {
		return "3", err
	}

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
	now := time.Now()
	tomorrow := now.Add(time.Hour * 24)
	// Update the user's session token in the database
	_, updateErr := DB.Exec("INSERT INTO session (token, user_id, created_at, expires_at) VALUES (?, ?, ?, ?);", token, userId, now, tomorrow)
	if updateErr != nil {
		return "6", updateErr
	}

	return token, nil
}

// Validates that session token does not already exist
func isSessionTokenValid(sessionToken string) (bool, error) {
	var exists bool
	err := DB.QueryRow("SELECT EXISTS(SELECT 1 FROM session WHERE token = ?);", sessionToken).Scan(&exists)
	if err != nil {
		return false, err
	}
	return exists, nil
}

func IsTokenStillActive(sessionToken string) (bool, error) {

	var expires time.Time
	err := DB.QueryRow("SELECT expires_at FROM session WHERE token = ?;", sessionToken).Scan(&expires)
	if err != nil {
		return false, err
	}
	valid := !expires.Before(time.Now())
	return valid, nil
}

func ClearSessionTokens() {
	_, err := DB.Exec("DELETE FROM session WHERE expires_at < ?", time.Now())
	if err != nil {
		log.Println(err.Error())
		return
	}
	log.Println("Successfully cleared session tokens from database")
}
func GetProcessNameById(id int) (string, error) {
	var name = ""

	row := DB.QueryRow("SELECT name FROM processes WHERE id=?", id)
	err := row.Scan(&name)
	if err != nil {
		return "", err
	}
	return name, nil

}
