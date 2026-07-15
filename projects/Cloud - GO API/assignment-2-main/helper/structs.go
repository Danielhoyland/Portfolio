package helper

// Country Used by the API in endpoints
type Country struct {
	Name       string
	Iso        string
	Borders    []string `json:",omitempty"`
	Percentage float64
	Year       int `json:",omitempty"`
}

// OutCountry only ever used while getting country data, which is instantly translated to the above struct
type OutCountry struct {
	NatName struct {
		Common string `json:"common"`
	} `json:"name"`
	Iso     string   `json:"cca3"`
	Borders []string `json:"borders"`
}

// Energy Used by the API in endpoints and no odin i didnt copy the message you did :)
type Energy struct {
	CountryName string
	Year        int
	Iso_code    string
	Presentage  float64 //renewables_cons_change_pct
}

// WebhookRegistration struct
type WebhookRegistration struct {
	WebhookID string `json:"webhook_id"`
	Url       string `json:"url"`
	Country   string `json:"country"`
	Calls     int    `json:"calls"`
}
