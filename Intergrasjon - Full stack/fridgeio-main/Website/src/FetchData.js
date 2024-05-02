import axios from 'axios';
import React, { useState, useEffect } from 'react';

// url for initial login
const loginurl = 'http://localhost:8080/login';
//url for adding a item
//url for deleting an item

// Make a request for login to retrieve fridge inventory data
const Login = () => {
  const [userData, setUserData] = useState(null);
  useEffect(() => {
    console.log('LOGIN FUNCTION RAN');
    return axios.post(loginurl, {
      email: "test@test.test",
      password: "test"
    })
    .then((response) => {
      console.log('Response from login:', response);
      setUserData(response.data);
      sessionStorage.setItem("Data", JSON.stringify(response.data));
    })
    .catch((error) => {
      console.log(error);
    });
  }, []) 
}



//Сan turn it off in Firefox like this in search bar about:config about:config -> security.fileuri.strict_origin_policy -> false
// https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS/Errors


/*-------------< MOCK RESPONSE >---------------*/

// exporting component so that you can use it in other parts of app
export {Login};