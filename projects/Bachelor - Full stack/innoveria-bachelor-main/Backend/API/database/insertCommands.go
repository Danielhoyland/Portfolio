package database

import "database/sql"

func insertUser(DB *sql.DB, email string, first_name string, last_name string, password string, permission int, company int) error {
	_, err := DB.Exec("INSERT INTO users (email, first_name, last_name, password, permission, company_id) VALUES (?, ?, ?, ?, ?, ?)",
		email, first_name, last_name, password, permission, company)

	if err != nil {
		return err
	}

	return nil
}
