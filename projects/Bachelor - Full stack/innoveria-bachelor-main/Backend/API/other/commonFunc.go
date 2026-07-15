package other

import (
	"fmt"
	"io"
	"net/http"
)

// NewHotdropDevice This function creates an output device with all needed parameters
func NewHotdropDevice(eui string, name string, desc string) OutputHotdrop {
	return OutputHotdrop{
		Eui:           eui,
		Name:          name,
		Description:   desc,
		Disabled:      false,
		SkipFcntCheck: true,
		JoinEui:       "00800000A0000767",
		AppId:         APPLICATION_ID,
		ProfileId:     DEVICE_PROFILE_ID,
	}
}

func NewGatewayDevice(eui string, name string) OutputGateway {
	return OutputGateway{
		Eui:           eui,
		Name:          name,
		TenantId:      TENANT_ID,
		StatsInterval: 30,
	}
}

func DoNewRequest(body io.Reader, url string, method string) error {
	//create request
	request, err := http.NewRequest(method, url, body)
	if err != nil {
		return err
	}
	request.Header.Add("Grpc-Metadata-Authorization", APIKEY)
	resp, err := http.DefaultClient.Do(request)
	if err != nil {
		return err
	} else if resp.StatusCode != 200 {
		b, _ := io.ReadAll(resp.Body)
		fmt.Println(string(b))
		return fmt.Errorf("something stopped the chirpstack from accepting the new device")
	}
	return nil
}
