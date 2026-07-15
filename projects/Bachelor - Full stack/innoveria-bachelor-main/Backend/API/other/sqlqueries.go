package other

import (
	"golang.org/x/crypto/bcrypt"
	"time"
)

/*
 * File where functions doing sql queries are saved
 */
/*FUNCTIONAL FUNCTIONS*/
func GetUserIDFromToken(token string) (int, error) {
	var userID int
	// Query to retrieve user ID based on session token
	err := DB.QueryRow("SELECT user_id FROM session WHERE token = ?", token).Scan(&userID)
	if err != nil {
		return 0, err
	}
	return userID, nil
}

func GetCompanyFromUserID(userID int) (int, error) {
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

/*USER FUNCTIONS*/
func InsertUser(token string, email string, firstName string, lastName string, password string, permission int) error {

	//gets userID
	userID, err := GetUserIDFromToken(token)
	if err != nil {
		return err
	}
	//gets the company id from user ID
	compID, err := GetCompanyFromUserID(userID)
	if err != nil {
		return err
	}

	_, err = DB.Exec("INSERT INTO users (email, first_name, last_name, password, permission, company_id) VALUES (?, ?, ?, ?, ?, ?)",
		email, firstName, lastName, password, permission, compID)

	if err != nil {
		return err
	}

	return nil
}
func EditUser(userID int, email string, firstName string, lastName string, password string, permission int) error {
	_, err := DB.Exec("UPDATE users SET email=?, first_name=?, last_name=?, password=?, permission=? WHERE id=?", email, firstName, lastName, password, permission, userID)
	if err != nil {
		return err
	}
	return nil
}
func DeleteUser(userID int) error {
	_, err := DB.Exec("DELETE FROM session WHERE user_id=?", userID)
	if err != nil {
		return err
	}

	_, err = DB.Exec("DELETE FROM users WHERE id=?", userID)
	if err != nil {
		return err
	}
	return nil
}

/*BUILDING FUNCTIONS*/
func InsertBuilding(token string, name string) error {
	//gets userID
	userID, err := GetUserIDFromToken(token)
	if err != nil {
		return err
	}
	//gets the company id from user ID
	compID, err := GetCompanyFromUserID(userID)
	if err != nil {
		return err
	}

	_, err = DB.Exec("INSERT INTO building (company_id, name) VALUES (?, ?)", compID, name)
	if err != nil {
		return err
	}

	return nil
}
func EditBuilding(id int, name string) error {
	_, err := DB.Exec("UPDATE building set name=? WHERE id=?", name, id)
	if err != nil {
		return err
	}

	return nil
}
func DeleteBuilding(id int) error {
	rows, err := DB.Query("SELECT eui FROM machine WHERE building_id = ?", id)
	if err != nil {
		return err
	}
	defer rows.Close()

	var euis []string
	for rows.Next() {
		var eui string
		if err := rows.Scan(&eui); err != nil {
			return err
		}
		euis = append(euis, eui)
	}
	if err := rows.Err(); err != nil {
		return err
	}

	for _, eui := range euis {
		_, err := DB.Exec("DELETE FROM machine_processes WHERE machine_id IN (SELECT eui FROM machine WHERE eui = ?)", eui)
		if err != nil {
			return err
		}
		_, err = DB.Exec("DELETE FROM sensorData WHERE eui=?", eui)
		if err != nil {
			return err
		}
	}

	_, err = DB.Exec("DELETE FROM machine WHERE building_id=?", id)
	if err != nil {
		return err
	}

	_, err = DB.Exec("DELETE FROM department WHERE building_id=?", id)
	if err != nil {
		return err
	}

	_, err = DB.Exec("DELETE FROM building WHERE id=?", id)
	if err != nil {
		return err
	}

	return nil
}
func GetBuildingIdByName(name string, compId int) (int, error) {
	var id = 0
	row := DB.QueryRow("SELECT id FROM building WHERE name=? AND company_id=?", name, compId)
	err := row.Scan(&id)
	if err != nil {
		return 0, err
	}
	return id, nil
}

/*DEPARTMENT FUNCTIONS*/
func InsertDepartment(buildingID int, name string) error {

	_, err := DB.Exec("INSERT INTO department (building_id, name) VALUES (?, ?)", buildingID, name)
	if err != nil {
		return err
	}
	return nil
}
func EditDepartment(id int, name string) error {
	_, err := DB.Exec("UPDATE department set name=? WHERE id=?", name, id)
	if err != nil {
		return err
	}

	return nil
}
func DeleteDepartment(id int) error {
	_, err := DB.Exec("UPDATE machine set department_id=NULL WHERE department_id=?", id)
	if err != nil {
		return err
	}

	_, err = DB.Exec("DELETE FROM department WHERE id=?", id)
	if err != nil {
		return err
	}

	return nil
}
func GetDepartmentIdByName(name string, compId int) (int, error) {
	var id = 0
	row := DB.QueryRow("SELECT id FROM department WHERE name=? AND building_id IN (SELECT id FROM building WHERE company_id=?)", name, compId)
	err := row.Scan(&id)
	if err != nil {
		return 0, err
	}
	return id, nil
}

/*ENOEK FUNCTIONS*/
func InsertEnoek(token string, measures EnokSuggestionMeasures) error {
	//gets userID
	userID, err := GetUserIDFromToken(token)
	if err != nil {
		return err
	}

	_, err = DB.Exec("INSERT INTO enoek_suggestion_measures (header, description, author, start_date, stop_date, active, process_id) VALUES (?, ?, ?, ?, ?, ?, ?)", measures.Header, measures.Description, userID, measures.StartDate, measures.StopDate, false, measures.ProcessID)
	if err != nil {
		return err
	}
	return nil
}
func EnoekJudgement(enoekID int, approved bool) error {
	_, err := DB.Exec("UPDATE enoek_suggestion_measures SET approved=? WHERE id=?", approved, enoekID)
	if err != nil {
		return err
	}
	return nil
}
func EnoekActiveOrNot(enoekID int, active bool) error {
	_, err := DB.Exec("UPDATE enoek_suggestion_measures SET active=? WHERE id=?", active, enoekID)
	if err != nil {
		return err
	}
	return nil
}
func DeleteEnoek(id int) error {
	_, err := DB.Exec("DELETE FROM enoek_suggestion_measures WHERE id=?", id)
	if err != nil {
		return err
	}
	return nil
}

/*COMPANY FUNCTIONS*/
func InsertCompany(name string, email string, password string) error {
	_, err := DB.Exec("INSERT INTO company (name) VALUES (?)", name)
	if err != nil {
		return err
	}
	row := DB.QueryRow("SELECT id FROM company WHERE name=?", name)
	var id = 0
	err = row.Scan(&id)
	if err != nil {
		return err
	}
	hash, err := bcrypt.GenerateFromPassword([]byte(password), 14)
	if err != nil {
		return err
	}
	_, err = DB.Exec("INSERT INTO users (email, first_name, last_name, password, permission, company_id) VALUES (?, 'admin', 'admin', ?, 0, ?)", email, string(hash), id)
	if err != nil {
		return err
	}
	return nil
}
func EditCompany(id int, name string) error {
	_, err := DB.Exec("UPDATE company SET name=? WHERE id=?", name, id)
	if err != nil {
		return err
	}
	return nil
}

/*SENSOR FUNCTIONS*/
func InsertSensor(machine Machine, processID int) error {
	if machine.DepartmentID != 0 {
		_, err := DB.Exec(`
		INSERT INTO machine (eui, name, expected_use, machineNr, building_id, department_id, voltage)
		VALUES (?, ?, ?, ?, ?, ?, ?)`,
			machine.EUI, machine.Name, machine.ExpectedUse, machine.MachineNr, machine.BuildingID, machine.DepartmentID, machine.Voltage)
		if err != nil {
			return err
		}
	} else {
		_, err := DB.Exec(`
		INSERT INTO machine (eui, name, expected_use, machineNr, building_id, voltage)
		VALUES (?, ?, ?, ?, ?, ?)`,
			machine.EUI, machine.Name, machine.ExpectedUse, machine.MachineNr, machine.BuildingID, machine.Voltage)
		if err != nil {
			return err
		}
	}

	// Check if processID is provided to associate the machine with a process
	if processID != 0 {
		// Insert into the machine_processes junction table
		_, err := DB.Exec(`
			INSERT INTO machine_processes (machine_id, processes_id)
			VALUES (?, ?)`,
			machine.EUI, processID)
		if err != nil {
			return err
		}
	}

	return nil
}
func EditSensor(machine Machine) error {
	if machine.DepartmentID != 0 {
		_, err := DB.Exec(`
		UPDATE machine set name=?, expected_use=?, machineNr=?, building_id=?, department_id=?, voltage=? WHERE eui=?`,
			machine.Name, machine.ExpectedUse, machine.MachineNr, machine.BuildingID, machine.DepartmentID, machine.Voltage, machine.EUI)
		if err != nil {
			return err
		}
	} else {
		_, err := DB.Exec(`
		UPDATE machine set name=?, expected_use=?, machineNr=?, building_id=?, department_id=?, voltage=? WHERE eui=?`,
			machine.Name, machine.ExpectedUse, machine.MachineNr, machine.BuildingID, nil, machine.Voltage, machine.EUI)
		if err != nil {
			return err
		}
	}
	return nil
}
func DeleteSensor(eui string) error {
	_, err := DB.Exec(`
		DELETE FROM sensorData WHERE eui=?`,
		eui)
	if err != nil {
		return err
	}
	_, err = DB.Exec(`
		DELETE FROM machine_processes WHERE machine_id=?`,
		eui)
	if err != nil {
		return err
	}
	_, err = DB.Exec(`
		DELETE FROM machine WHERE eui=?`,
		eui)
	if err != nil {
		return err
	}
	return nil
}

func InsertSensorData(data SensorData) error {
	_, err := DB.Exec(`
		INSERT INTO sensorData (eui, Acumulation, AVG_current, Offset_max, Offset_min, Voltage, Temprature, date_time)
		VALUES (?, ?, ?, ?, ?, ?, ?, ?)`,
		data.EUI, data.Accumulation, data.AVGCurrent, data.OffsetMax, data.OffsetMin, data.Voltage, data.Temperature, data.DateTime)
	if err != nil {
		return err
	}
	return nil
}

// clears ALL sensordata, from all companies, scary!
func ClearSensorData() error {
	_, err := DB.Exec("DELETE FROM sensorData")
	if err != nil {
		return err
	}
	return nil
}

/*PROCESS FUNCTIONS*/
func InsertProcess(token string, processes Processes) error {
	//gets userID
	userID, err := GetUserIDFromToken(token)
	if err != nil {
		return err
	}
	//gets the company id from user ID
	compID, err := GetCompanyFromUserID(userID)
	if err != nil {
		return err
	}

	_, err = DB.Exec("INSERT INTO processes (name, description, company_id) VALUES (?, ?, ?)", processes.Name, processes.Description, compID)
	if err != nil {
		return err
	}
	return nil
}
func EditProcess(processes Processes) error {
	_, err := DB.Exec("UPDATE processes set name=?, description=? WHERE id=?", processes.Name, processes.Description, processes.ID)
	if err != nil {
		return err
	}

	return nil
}
func DeleteProcess(id int) error {

	_, err := DB.Exec("DELETE FROM enoek_suggestion_measures WHERE process_id=?", id)
	if err != nil {
		return err
	}

	_, err = DB.Exec("DELETE FROM machine_processes WHERE processes_id=?", id)
	if err != nil {
		return err
	}

	_, err = DB.Exec("DELETE FROM processes WHERE id=?", id)
	if err != nil {
		return err
	}

	return nil
}

func GetProcessIDByName(processes Processes) (int, error) {
	var id = 0
	if processes.Description != "" {
		row := DB.QueryRow("SELECT id FROM processes WHERE name=? AND description=? AND company_id=?", processes.Name, processes.Description, processes.CompanyID)
		err := row.Scan(&id)
		if err != nil {
			return 0, err
		}
		return id, nil
	} else {
		row := DB.QueryRow("SELECT id FROM processes WHERE name=? AND company_id=?", processes.Name, processes.CompanyID)
		err := row.Scan(&id)
		if err != nil {
			return 0, err
		}
		return id, nil
	}
}

/*SENSOR PROCESS FUNCTIONS*/
func AddSensorToProcess(maPro MachineProcesses) error {
	_, err := DB.Exec("INSERT INTO machine_processes (machine_id, processes_id) VALUES (?, ?)", maPro.MachineID, maPro.ProcessesID)
	if err != nil {
		return err
	}
	return nil
}
func EditSensorToProcess(addOrNot bool, processId int, machineEUI string) error {
	if addOrNot {
		_, err := DB.Exec("INSERT INTO machine_processes (machine_id, processes_id) VALUES (?, ?)", machineEUI, processId)
		if err != nil {
			return err
		}
	} else {
		_, err := DB.Exec("DELETE FROM machine_processes WHERE machine_id=? AND processes_id=? ", machineEUI, processId)
		if err != nil {
			return err
		}
	}

	return nil
}
func DeleteSensorToProcess(maPro MachineProcesses) error {
	_, err := DB.Exec("DELETE FROM machine_processes WHERE machine_id=? AND processes_id=? ", maPro.MachineID, maPro.ProcessesID)
	if err != nil {
		return err
	}
	return nil
}

/*GATEWAY FUNCTIONS*/
func AddGateway(token string, eui string, name string) error {
	//gets userID
	userID, err := GetUserIDFromToken(token)
	if err != nil {
		return err
	}
	//gets the company id from user ID
	compID, err := GetCompanyFromUserID(userID)
	if err != nil {
		return err
	}
	_, err = DB.Exec("INSERT INTO gateway (eui, company_id, name) VALUES (?, ?, ?)", eui, compID, name)
	if err != nil {
		return err
	}
	return nil
}
func EditGateway(newEui string, oldEui string, newName string, oldName string, compId int) error {
	_, err := DB.Exec("UPDATE gateway SET eui=?, name=? where eui=? AND name=? AND company_id=?", newEui, newName, oldEui, oldName, compId)
	if err != nil {
		return err
	}
	return nil
}
func DeleteGateway(eui string) error {
	_, err := DB.Exec("DELETE FROM gateway where eui=?", eui)
	if err != nil {
		return err
	}
	return nil
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
