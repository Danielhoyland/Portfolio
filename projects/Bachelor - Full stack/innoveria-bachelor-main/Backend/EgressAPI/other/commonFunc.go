package other

import (
	"encoding/json"
	"io"
)

// ValidateRequest returns the session token from a given request body (decodes it) and returns "" (empty string) and error on fail
func ValidateRequest(body io.Reader) (string, error) {
	token := struct {
		SessionToken string `json:"sessionToken"`
	}{}
	decoder := json.NewDecoder(body)
	err := decoder.Decode(&token)
	if err != nil {
		return "", err
	}
	valid, err := IsTokenStillActive(token.SessionToken)
	if err != nil && valid {
		return "", err
	}
	return token.SessionToken, nil
}
