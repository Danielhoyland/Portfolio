import React, {useEffect, useState, useRef} from 'react'; //importing react for react components

import './Login.css';
import axios from 'axios';
import Carrot from "../icons/carrot-vegetable-icon.svg"
import Fridge from "../icons/fridge-icon.svg"
import Settings from "../icons/setting-line-icon.svg"
import Book from "../icons/book-icon.svg"

const loginurl = process.env.REACT_APP_API_URL + '/sessionToken';
const newuserurl = process.env.REACT_APP_API_URL + '/new-user';

const Login = () => {
    //the data needed for the login page, useState is a hook that reloads the elements that uses the values store within data
    const [data, setData] = useState({
        email: "", password: "", username: ""
    });
    //when changing anything in the text fields we call teh handle change
    const handleChange = (e) => {
        //event stores both the new value and what field had the change
        const value = e.target.value;
        setData({
            //all the input fields are either called email, password or username, so I can use the e.target.name to determine what values to change
            ...data, [e.target.name]: value
        });
    };
    //response is feedback given back to the user if something is wrong with the login
    const [response, setResponse] = useState('');

    //the login page consist of a login part and a newAccount part, I store this in section storage, so it remembers even after reloads
    let newAccount = sessionStorage.getItem("newAccount");
    if(newAccount == "" || newAccount == "true"){
        newAccount = true
    }else{
        newAccount = false
    }


    const handlePageChange = ()  => {
        //when changing between login or newAccount
        let temp = sessionStorage.getItem("newAccount") !== "true";
        sessionStorage.removeItem("newAccount")
        sessionStorage.setItem("newAccount", temp.toString())
        //reloads the page
        window.location.href = '/login';
    }

    const handleSubmit = (e) => {
        e.preventDefault();
        const userData = {
            email: data.email,
            password: data.password
        };
        //axios.post takes the loginUrl and the userData and converts it into a json
        axios.post(loginurl, userData, {headers: { 'Content-Type': 'application/json',},}).then((response) => {
            //then it read the response
            console.log(response.data)
            if(response.status === 200){
                //if the response is ok then we store the session token given by the backend
                //this will be used later for communicating between front and backend securely
                sessionStorage.removeItem("sessionToken")
                sessionStorage.setItem("sessionToken", response.data)
                //clears the response
                setResponse('')
                //transfers the suser to teh main page
                window.location.href = '/myfridge';
            }else{
                //tels the user something went bad with both a console log and a response string
                console.log("Login not valid something went wrong")
                setResponse("Login not valid something went wrong")
            }
        }).catch((error) => {
            //error handeling
            if (error.response) {
                console.log(error.response)
                console.log("server responded")
                setResponse('Wrong username or password')
                console.log("hey", loginurl, userData)
            } else if (error.request) {
                console.log("network error")
                setResponse('Unable to reach server')
            } else {
                console.log(error)
            }
        });
    }

    const handleNewUser = (e) => {
        e.preventDefault();
        const userData = {
            email: data.email,
            username: data.username,
            password: data.password
        };
        //same as before but calling the new user api instead of the login one
        axios.post(newuserurl, userData, {headers: { 'Content-Type': 'application/json',},})
            .then((response) => {
                console.log(response.status, response.data.token)
                console.log(response.data)
                if(response.status === 200){
                    //if the response was a success I simply reuse the login with the new parameters
                    handleSubmit(e);
                }else{
                    console.log("Something went wrong with creating the new user")
                }
            })
            .catch((error) => {
                if (error.response) {
                    console.log(error.response)
                    console.log("server responded")
                }else if (error.request) {
                    console.log("network error")
                } else {
                    console.log(error)
                }
            });
    }

    //this code is for the fancy background animation
    const Background = () => {
        //figures out if the page is in mobile or laptop
        const rows = window.innerWidth < 768 ? 9:5
        const cols = window.innerWidth < 768 ? 3:5;
        useEffect(() => {
            //useEffect is another hook that constantly runs and has a built-in timer
            const svgContainers = document.querySelectorAll('.svg-container');
            svgContainers.forEach((container, idnex) => {
                const delay = (idnex%cols) * 0.5;
                //animates the grid
                container.style.animation = `rotate 5s ease-in-out infinite ${delay}s`;
            })
        });
        return (
            <div className="background-container">
                {generateGridOfSVGs(rows, cols)}
            </div>
        );
    };

    function generateGridOfSVGs(rows, cols) {
        //generates the grid
        const svgArray = [];
        for (let row = 0; row < rows; row++){
            for(let col = 0; col < cols; col++){
                //pushes each image to the grid
                svgArray.push(
                    <div key={`svg-${row}-${col}`} className="svg-container">
                        <img src={Fridge} id="fridge" alt={"Fridge"}/>
                    </div>
                );
            }
        }
        return svgArray;
    }


    return (
        <div>
            <div className="container">
                <h2>{newAccount ? ("New Account") : ("Login")}</h2>
                {newAccount ? (
                    <form onSubmit={handleNewUser}>
                        <div className="form-group">
                        <label htmlFor="text">Username:</label>
                        <input
                            type = "text"
                            id="username"
                            name="username"
                            value={data.username}
                            onChange={handleChange}
                            placeholder="Enter your User Name" required/>
                        </div>
                        <div className="form-group">
                            <label htmlFor="email">Email:</label>
                            <input
                                type="email"
                                id="email"
                                name="email"
                                value={data.email}
                                onChange={handleChange}
                                placeholder="Enter your email" required/>
                        </div>
                        <div className="form-group">
                            <label htmlFor="password">Password:</label>
                            <input
                                type="password"
                                id="password"
                                name="password"
                                value={data.password}
                                onChange={handleChange}
                                placeholder="Enter your password" required/>
                        </div>
                        <button id = "button_submit" type="submit">Creat New account</button>
                        <div className="response">{response}</div>
                    </form>
                ) : (
                    <form onSubmit={handleSubmit}>
                        <div className="form-group">
                            <label htmlFor="email">Email:</label>
                            <input
                                type="email"
                                id="email"
                                name="email"
                                value={data.email}
                                onChange={handleChange}
                                placeholder="Enter your email" required/>
                        </div>
                        <div className="form-group">
                            <label htmlFor="password">Password:</label>
                            <input
                                type="password"
                                id="password"
                                name="password"
                                value={data.password}
                                onChange={handleChange}
                                placeholder="Enter your password" required/>
                        </div>
                        <button id = "button_submit" type="submit">Login</button>
                        <div className="response">{response}</div>
                    </form>
                )}
                <form onSubmit={handlePageChange}>
                    <button id = "button_submit" type="submit">{newAccount ? ("Back to login") : ("Create new account")}</button>
                </form>
            </div>

            {/*Background*/}
            {Background()}
        </div>
    );
}

// exporting component so that you can use it in other parts of app
export default Login;