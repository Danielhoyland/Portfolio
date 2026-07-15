package test

import (
	"assignment-2/helper"
	"reflect"
	"testing"
	"time"
)

var startTime time.Time

// add testing for GetEnergy here
func TestGetEnergy(t *testing.T) {
	rr, _ := helper.GetEnergy("NOR", "1998", "1999")
	expected := "Norway"
	if rr[0].CountryName != expected {
		t.Errorf("handler returned unexpected body: got %v want %v", rr[0].CountryName, expected)
	}
}
func TestGetCountry(t *testing.T) {
	// Test the function for a valid input and a valid response
	expected := []helper.Country{
		{Name: "Afghanistan", Iso: "AFG", Borders: []string{"IRN", "PAK", "TKM", "UZB", "TJK", "CHN"}, Percentage: 82.906, Year: 2018},
		{Name: "Afghanistan", Iso: "AFG", Borders: []string{"IRN", "PAK", "TKM", "UZB", "TJK", "CHN"}, Percentage: 83.178, Year: 2019},
		{Name: "Afghanistan", Iso: "AFG", Borders: []string{"IRN", "PAK", "TKM", "UZB", "TJK", "CHN"}, Percentage: 85, Year: 2020},
	}

	country := helper.GetCountry("afg", "2018", "2020")
	if !reflect.DeepEqual(country, expected) {
		t.Errorf("GetCountry returned unexpected value: got %v want %v", country, expected)
	}

}
func TestToCountry(t *testing.T) {
	o := []helper.OutCountry{
		{NatName: struct {
			Common string `json:"common"`
		}(struct{ Common string }{Common: "Afghanistan"}), Iso: "AFG", Borders: []string{"IRN", "PAK", "TKM", "UZB", "TJK", "CHN"}},
		{NatName: struct {
			Common string `json:"common"`
		}(struct{ Common string }{Common: "Norway"}), Iso: "NOR", Borders: []string{"FIN", "SWE", "RUS"}},
	}

	expected := []helper.Country{
		{Name: "Afghanistan", Iso: "AFG", Borders: []string{"IRN", "PAK", "TKM", "UZB", "TJK", "CHN"}},
		{Name: "Norway", Iso: "NOR", Borders: []string{"FIN", "SWE", "RUS"}},
	}

	result := helper.ToCountry(o)

	if !reflect.DeepEqual(result, expected) {
		t.Errorf("Expected %v, but got %v", expected, result)
	}
}
func TestToCountry2(t *testing.T) {
	// Define a sample input Country and Energy
	country := helper.Country{Name: "Norway", Iso: "NOR", Borders: []string{"FIN", "SWE", "RUS"}}
	energy := []helper.Energy{{Year: 2018, Presentage: 60.0}, {Year: 2019, Presentage: 65.0}, {Year: 2020, Presentage: 70.0}}

	// Call the toCountry2 function
	result := helper.ToCountry2(country, energy)

	// Define the expected output
	expected := []helper.Country{
		{Name: "Norway", Iso: "NOR", Borders: []string{"FIN", "SWE", "RUS"}, Percentage: 60.0, Year: 2018},
		{Name: "Norway", Iso: "NOR", Borders: []string{"FIN", "SWE", "RUS"}, Percentage: 65.0, Year: 2019},
		{Name: "Norway", Iso: "NOR", Borders: []string{"FIN", "SWE", "RUS"}, Percentage: 70.0, Year: 2020},
	}

	// Compare the actual and expected output
	if !reflect.DeepEqual(result, expected) {
		t.Errorf("Unexpected result. Got: %v, expected: %v", result, expected)
	}
}
func TestInit(t *testing.T) {
	// Call the function to set the start time
	helper.Init()

	// Get the current time
	now := time.Now()

	// Check if the startTime variable is equal to the current time
	if startTime.After(now) {
		t.Errorf("startTime is not set to the current time")
	}
}
func TestUptime(t *testing.T) {
	// Call the Init function to set the start time
	helper.Init()

	// Wait for 1 second
	time.Sleep(1 * time.Second)

	// Get the uptime
	uptime := helper.Uptime()

	// Check if the uptime is greater than or equal to 1 second
	if uptime < 1*time.Second {
		t.Errorf("Uptime is less than 1 second: %v", uptime)
	}
}
func TestCalculateAveragePercentages(t *testing.T) {
	// Create a sample input with three countries
	inputCountries := []helper.Country{
		{Name: "Norway", Iso: "NOR", Percentage: 80.0, Year: 1},
		{Name: "Sweden", Iso: "SWE", Percentage: 75.0, Year: 1},
		{Name: "Finland", Iso: "FIN", Percentage: 70.0, Year: 1},
	}

	// Calculate the expected output with the average percentages
	expectedOutput := []helper.Country{
		{Name: "Finland", Iso: "FIN", Percentage: 70.0, Year: 0},
		{Name: "Norway", Iso: "NOR", Percentage: 80.0, Year: 0},
		{Name: "Sweden", Iso: "SWE", Percentage: 75.0, Year: 0},
	}

	// Call the function with the input countries
	output := helper.CalculateAveragePercentages(inputCountries)

	// Check that the output has the expected length
	if len(output) != len(expectedOutput) {
		t.Errorf("Output length does not match expected length")
		return
	}

	// Check that each output country matches the corresponding expected country
	for i := 0; i < len(output); i++ {
		if output[i].Name != expectedOutput[i].Name ||
			output[i].Iso != expectedOutput[i].Iso ||
			output[i].Percentage != expectedOutput[i].Percentage ||
			output[i].Year != expectedOutput[i].Year {
			t.Errorf("Output does not match expected output at index %d", i)
		}
	}
}
