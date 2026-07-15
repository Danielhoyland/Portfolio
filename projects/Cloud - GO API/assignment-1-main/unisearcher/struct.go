package unisearcher

type University struct {
	Name        string   `json:"name"`
	CountryName string   `json:"country"`
	Iscode      string   `json:"alpha_two_code"`
	Webpages    []string `json:"web_pages"`
	Country     `json:",inline"`
}

type Country struct {
	Name struct {
		Common string `json:"common"`
	} `json:"name"`
	Languages     map[string]interface{} `json:"languages"`
	Borders       []string               `json:"borders,omitempty"`
	Iscode        string                 `json:"cca2,omitempty"`
	Is3code       string                 `json:"cca3,omitempty"`
	Map           map[string]string      `json:"maps,omitempty"`
	OpenStreetMap string                 `json:"map,omitempty"`
}
type NeighbourUnisResponse struct {
	Country string `json:"country"`
	Uni     string `json:"university"`
	Limit   string `json:"limit"`
}
