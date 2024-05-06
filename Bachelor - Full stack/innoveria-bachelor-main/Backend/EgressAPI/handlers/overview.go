package handlers

import (
	"EgressAPI/other"
	"encoding/json"
	"log"
	"net/http"
	"strconv"
	"time"
)

func OverviewHandler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Access-Control-Allow-Headers", "Content-Type")
	w.Header().Set("Access-Control-Allow-Origin", ""+other.WebsiteURL)
	w.Header().Set("Access-Control-Allow-Methods", "POST, OPTIONS")
	if r.Method == http.MethodPost {

		token, err := other.ValidateRequest(r.Body)
		if err != nil {
			w.WriteHeader(http.StatusUnauthorized)
			return
		}
		overviewData, err := other.GetMachineDetailsForUser(token)
		if err != nil {
			// Log the error or handle it appropriately
			log.Printf("Error getting machine details: %v", err)
			w.WriteHeader(http.StatusInternalServerError)
			return
		}
		//sort the data into intervals, and since day x
		params := r.URL.Query()

		since, err := strconv.ParseInt(params.Get("since"), 10, 64)
		if err != nil {
			log.Println(err.Error())
			w.WriteHeader(http.StatusBadRequest)
			return
		}

		interval, err := strconv.ParseInt(params.Get("interval"), 10, 64)
		if err != nil {
			log.Println(err.Error())
			w.WriteHeader(http.StatusBadRequest)
			return
		}

		start, err := strconv.ParseInt(params.Get("start"), 10, 64)
		if err != nil {
			log.Println(err.Error())
			w.WriteHeader(http.StatusBadRequest)
			return
		}

		overviewData, err = sortData(overviewData, interval, since, start)
		datasets := setIntoDatasets(overviewData)
		if err != nil {
			log.Println(err.Error())
			w.WriteHeader(http.StatusInternalServerError)
			return
		}
		// Encode and write JSON response
		err = json.NewEncoder(w).Encode(datasets)
		if err != nil {
			// Log the error or handle it appropriately
			log.Printf("Error encoding JSON: %v", err)
			w.WriteHeader(http.StatusInternalServerError)
			return
		}
	} else if r.Method == http.MethodOptions {

	} else {
		// Respond with 418 I'm a teapot for non-GET requests
		w.WriteHeader(http.StatusMethodNotAllowed)
	}
}

// sortData sorts the machinedetails into the given interval, from "since" days ago to "start" days ago (since has to be greater than start)
// The duration is at the END of the interval, i.e a given avg amp hour is valid the <interval> minutes before the timestamp
func sortData(machines []other.MachineDetails, interval int64, since int64, start int64) ([]other.MachineDetails, error) {
	var intervalMinutes = time.Minute * time.Duration(interval)
	var intervalList []time.Time
	startTime := time.Now().Add(-time.Hour * 24 * time.Duration(since)) // when to start the packets
	endTime := time.Now().Add(-time.Hour * 24 * time.Duration(start))   // when to stop the packets
	var currentIntervalTime = startTime
	//create list of timestamps so each of the machines use the same intervals
	for {
		currentIntervalTime = currentIntervalTime.Add(intervalMinutes)
		intervalList = append(intervalList, currentIntervalTime)

		if currentIntervalTime.Add(intervalMinutes).After(endTime) {
			break
		}
	}
	//loop over each machine using a for loop
	for k := range machines {
		quickSort(machines[k].SensorDataList, 0, len(machines[k].SensorDataList)-1)

		var packetList []other.SensorData     //new list of packets
		var tempPacketList []other.SensorData //temporary list of packets, to make sure it is averaged correctly
		var i = 0

		for _, tInterval := range intervalList {

			for i < len(machines[k].SensorDataList) {
				//println(i)
				if machines[k].SensorDataList[i].DateTime.Before(startTime) { //if the packet has not yet reached the start time, go to next packet
					i++
					continue
				}
				if machines[k].SensorDataList[i].DateTime.After(tInterval) { //if the packet has passed the current interval, go to next interval
					break
				}

				//if nothing else, the packet is within the current interval
				tempPacketList = append(tempPacketList, machines[k].SensorDataList[i])
				i++
			}

			var acu float64 = 0
			var avgCurr float64 = 0
			var offMAX float64 = 0
			var offMIN float64 = 0
			var volt float64 = 0
			var temp float64 = 0
			if len(tempPacketList) > 0 {
				//summarize the packet info, to create average below
				for _, p := range tempPacketList {
					acu = p.Acumulation
					avgCurr += p.AVGCurrent
					offMAX += p.OffsetMax
					offMIN += p.OffsetMin
					volt += p.Voltage
					temp += p.Temperature
				}
				//create a packet that contains the averages of the previous packets data, even if it is zero (in the else clause), so every machine has equal len of data
				newPacket := other.SensorData{
					Acumulation: acu,
					AVGCurrent:  avgCurr / float64(len(tempPacketList)),
					OffsetMax:   offMAX / float64(len(tempPacketList)),
					OffsetMin:   offMIN / float64(len(tempPacketList)),
					Voltage:     volt / float64(len(tempPacketList)),
					Temperature: temp / float64(len(tempPacketList)),
					DateTime:    tInterval,
				}
				packetList = append(packetList, newPacket)
			} else {
				newPacket := other.SensorData{
					Acumulation: acu,
					AVGCurrent:  avgCurr,
					OffsetMax:   offMAX,
					OffsetMin:   offMIN,
					Voltage:     volt,
					Temperature: temp,
					DateTime:    tInterval,
				}
				packetList = append(packetList, newPacket)
			}
			tempPacketList = nil
		}
		//set the sensorData list to the machine
		machines[k].SensorDataList = packetList
		//empty the list, so old data is not reused
		packetList = nil
	}
	return machines, nil
}

// sorts the list of SensorData by time, in ascending order, index low being the earliest time, index high being the latest
// implementation of quicksort

func quickSort(arr []other.SensorData, low int, high int) {
	if low < high {
		pi := partion(arr, low, high)

		// Recursively sort elements before partition and after partition
		quickSort(arr, low, pi-1)
		quickSort(arr, pi+1, high)
	}
}

func partion(arr []other.SensorData, low int, high int) int {
	pivot := arr[high]
	i := low - 1

	for j := low; j < high; j++ {
		if arr[j].DateTime.Before(pivot.DateTime) {
			i++

			arr[i], arr[j] = arr[j], arr[i]
		}
	}

	arr[i+1], arr[high] = arr[high], arr[i+1]
	return i + 1
}

// function to transform machinedetails struct into data for frontend
func setIntoDatasets(Machines []other.MachineDetails) other.OverviewData {
	oData := other.OverviewData{}
	doneOnce := false

	for _, machine := range Machines {
		dataset := createDataset(machine)
		OMachine := other.OMachine{Name: machine.Machine.Name, EUI: machine.Machine.EUI, Dataset: dataset}
		if !containsBuilding(machine.Building.Name, oData.Buildings) {
			oData.Buildings = append(oData.Buildings, other.OBuilding{Name: machine.Building.Name})
		}
		for idx, building := range oData.Buildings {
			if machine.Building.Name == building.Name {
				if !containsDepartment(machine.Department.Name, building.Departments) {
					if machine.Department.Name == "" && !containsDepartment("No department", building.Departments) {
						oData.Buildings[idx].Departments = append(building.Departments, other.ODepartment{Name: "No department"})
					} else {
						oData.Buildings[idx].Departments = append(building.Departments, other.ODepartment{Name: machine.Department.Name})
					}
				}
			}
		}
		for idx, building := range oData.Buildings {
			for dIdx, department := range building.Departments {
				if department.Name == machine.Department.Name || (department.Name == "No department" && machine.Department.Name == "") {
					oData.Buildings[idx].Departments[dIdx].Machines = append(department.Machines, OMachine)
					if !doneOnce {
						var timeList []time.Time
						for _, data := range machine.SensorDataList {
							timeList = append(timeList, data.DateTime)
						}
						oData.Labels = timeList
						doneOnce = true //only set the labels on the first machine
					}
				}
			}
		}
		//loop over all the current machines associated process
		for _, process := range machine.AssociatedProcesses {
			found, idx := containsProcess(process.Name, oData.Processes)
			if !found {
				if process.Name != "" {
					tempProcess := other.OProcess{Name: process.Name}
					tempProcess.Machines = append(tempProcess.Machines, OMachine)
					oData.Processes = append(oData.Processes, tempProcess)
				}
			} else {
				oData.Processes[idx].Machines = append(oData.Processes[idx].Machines, OMachine)
			}
		}
	}

	for bIdx, building := range oData.Buildings {
		buildingDataset := other.Dataset{Label: building.Name}
		var allDepartments []other.Dataset

		for dIdx, department := range building.Departments {
			var allMachines []other.Dataset
			for _, machine := range department.Machines {
				allMachines = append(allMachines, machine.Dataset)
			}
			oData.Buildings[bIdx].Departments[dIdx].Dataset = avgDatasets(allMachines)
			oData.Buildings[bIdx].Departments[dIdx].Dataset.Label = department.Name
			allDepartments = append(allDepartments, oData.Buildings[bIdx].Departments[dIdx].Dataset)
		}
		buildingDataset = avgDatasets(allDepartments)
		buildingDataset.Label = building.Name
		oData.Buildings[bIdx].Dataset = buildingDataset
	}

	for pIdx, process := range oData.Processes {
		processDataset := other.Dataset{Label: process.Name}
		var allMachines []other.Dataset

		for _, machine := range process.Machines {
			allMachines = append(allMachines, machine.Dataset)
		}
		processDataset = avgDatasets(allMachines)
		oData.Processes[pIdx].Dataset = processDataset
	}

	return oData
}

// turns a "MachineDetails" into a dataset
func createDataset(details other.MachineDetails) other.Dataset {
	dataset := other.Dataset{Label: details.Machine.Name}
	for _, data := range details.SensorDataList {
		dataset.Data = append(dataset.Data, data.AVGCurrent*float64(details.Machine.Voltage))
	}
	return dataset
}

// averages a list of datasets
func avgDatasets(datasets []other.Dataset) other.Dataset {
	retDataset := other.Dataset{}
	total := 0
	for _, dataset := range datasets {
		for idx, data := range dataset.Data {
			if len(retDataset.Data) < idx+1 {
				retDataset.Data = append(retDataset.Data, data)
			} else {
				retDataset.Data[idx] += data
			}
		}
		total++
	}
	for _, data := range retDataset.Data {
		data = data / float64(total)
	}
	return retDataset
}

func containsBuilding(name string, buildings []other.OBuilding) bool {
	for _, building := range buildings {
		if building.Name == name {
			return true
		}
	}
	return false
}

func containsDepartment(name string, departments []other.ODepartment) bool {
	for _, building := range departments {
		if building.Name == name {
			return true
		}
	}
	return false
}

func containsProcess(name string, processes []other.OProcess) (bool, int) {
	for idx, building := range processes {
		if building.Name == name {
			return true, idx
		}
	}
	return false, -1
}
