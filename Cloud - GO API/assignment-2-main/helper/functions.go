package helper

import (
	"encoding/csv"
	"encoding/json"
	"fmt"
	"net/http"
	"os"
	"path/filepath"
	"sort"
	"strconv"
	"time"
)

// GetCountry returns a list of the "Country" struct, from a given year, to a given year
func GetCountry(iso string, years string, yeare string) []Country {
	// Build the URL based on the input parameters
	url := ""
	if iso == "all" || iso == "allCurrent" {
		url = RESTC + "all"
	} else {
		url = RESTC + "alpha/" + iso
	}
	// Make an HTTP request to the REST API
	res, err := http.Get(url)
	if err != nil {
		// If there is an error, return nil
		return nil
	}

	// Parse the response into an array of "OutCountry" structs
	c := []OutCountry{}
	decoder := json.NewDecoder(res.Body)
	if err := decoder.Decode(&c); err != nil {
		fmt.Println(res.Status)
		return nil
	}
	// Initialize the return array
	retCountries := []Country{}
	// If there are countries in the response
	if len(c) > 0 {
		// Translate the "OutCountry" structs into "Country" structs
		partialc := ToCountry(c)
		// For each country, get the energy data
		for _, co := range partialc {
			energy, err := GetEnergy(co.Iso, years, yeare)
			if err != nil {
				return nil
			}
			// Merge the country data with the energy data
			countries := ToCountry2(co, energy)
			retCountries = append(retCountries, countries...)
		}
		// If the request was for all countries, calculate the average percentages and return a single country struct
		if iso == "all" {
			var meanCountry = CalculateAveragePercentages(retCountries)
			return meanCountry
		} else {
			// Otherwise, return the list of country structs
			return retCountries
		}
	} else {
		// If there are no countries in the response, return nil
		return nil
	}
}

// GetEnergy gets energy from isocode, returns a list of energy percentages from one year to another
func GetEnergy(iso string, year_s string, year_e string) ([]Energy, error) {
	// Get the current working directory of the project
	wd, err := os.Getwd()
	if err != nil {
		return nil, err
	}

	// Construct the path to the CSV file using the project directory
	var energyFilePath = ""
	if filepath.Base(wd) == "assignment-2" {
		energyFilePath = filepath.Join(wd, ENERGY)
	} else {
		energyFilePath = filepath.Join(filepath.Dir(wd), ENERGY)
	}
	f, err := os.Open(energyFilePath)
	if err != nil {
		fmt.Printf("Error open CSV file: %s\n", err)
		return nil, err
	}
	defer f.Close()

	r := csv.NewReader(f)
	rows, err := r.ReadAll()
	if err != nil {
		fmt.Printf("Error reading CSV file: %s\n", err)
		return nil, err
	}

	years, _ := strconv.Atoi(year_s)
	yeare, _ := strconv.Atoi(year_e)
	energyData := []Energy{}
	for _, row := range rows {
		year, _ := strconv.Atoi(row[1])
		// Check if year is within the given range
		if yeare != 0 {
			if iso == row[2] && years <= year && yeare >= year {
				// Parse the renewable energy consumption percentage from the row
				renewables_consumption, _ := strconv.ParseFloat(row[111], 64)

				// Add a new Energy struct to the energyData slice
				energyData = append(energyData, Energy{
					CountryName: row[0],
					Year:        year,
					Iso_code:    row[2],
					Presentage:  renewables_consumption,
				})
			}
		} else {
			// If no end year is specified, use 3000 as the upper bound
			if iso == row[2] && years <= year && 3000 >= year {
				renewables_consumption, _ := strconv.ParseFloat(row[111], 64)

				energyData = append(energyData, Energy{
					CountryName: row[0],
					Year:        year,
					Iso_code:    row[2],
					Presentage:  renewables_consumption,
				})
			}
		}
	}
	return energyData, nil
}

// translates an output country to a country struct
func ToCountry(o []OutCountry) []Country {
	countries := []Country{}
	for _, c := range o {
		// Create a new Country struct from the OutCountry struct
		countries = append(countries, Country{Iso: c.Iso, Name: c.NatName.Common, Borders: c.Borders})
	}
	return countries
}

// takes a country and a list of energy structs and merges them
func ToCountry2(country Country, energy []Energy) []Country {
	retCountry := []Country{}
	for _, c := range energy {
		// Add the Percentage and Year fields from the Energy struct to the Country struct
		country.Percentage = c.Presentage
		country.Year = c.Year
		retCountry = append(retCountry, country)
	}
	return retCountry
}

// Timer for status
var startTime time.Time

// Init initializes the startTime variable to the current time
func Init() {
	startTime = time.Now()
}

// Uptime returns the duration since the startTime variable was initialized
func Uptime() time.Duration {
	return time.Since(startTime)
}

func CalculateAveragePercentages(countries []Country) []Country {
	// Create a map to store the total percentage and count for each country
	countryTotals := make(map[string]Country)

	// Loop through all countries and accumulate the total percentage and count
	for _, country := range countries {
		if _, ok := countryTotals[country.Iso]; !ok {
			countryTotals[country.Iso] = Country{Name: country.Name, Iso: country.Iso}
		}

		// Retrieve the struct from the map, modify its fields, and store it back in the map
		c := countryTotals[country.Iso]
		c.Percentage += country.Percentage
		c.Year++
		countryTotals[country.Iso] = c
	}

	// Create a slice to store the output countries
	outputCountries := make([]Country, 0, len(countryTotals))

	// Loop through the accumulated totals and calculate the average percentage for each country
	for _, country := range countryTotals {
		if country.Year > 0 {
			country.Percentage = country.Percentage / float64(country.Year)
		}
		country.Year = 0
		outputCountries = append(outputCountries, country)
	}
	// Sort the output countries by name
	sort.Slice(outputCountries, func(i, j int) bool {
		return outputCountries[i].Name < outputCountries[j].Name
	})

	return outputCountries
}
