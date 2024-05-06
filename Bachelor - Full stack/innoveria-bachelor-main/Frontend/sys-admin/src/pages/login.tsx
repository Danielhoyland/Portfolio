import { useState } from 'react'
import { useNavigate } from "react-router-dom";
import { Button } from "@/components/ui/button";
import "./login.css"
import axios from 'axios';

import {
    Card,
    CardContent,
    CardDescription,
    CardFooter,
    CardHeader,
    CardTitle,
} from "@/components/ui/card"

//var IngressAPI: String = import.meta.env.VITE_IAPI_URL
const EgressAPI = import.meta.env.VITE_EAPI_URL


function Login() {

    const [email, setEmail] = useState("")
    const [pass, setPass] = useState("")

    const navigate = useNavigate();

    const loader = async () => {
        navigate("/admin")
    };

    return(
        <div id="loginpage"> {/* !! READ ABOUT USING "zod" https://zod.dev to allow client side validation && needed to make SHADUI form !! */}
            <div id="logincard">
                <Card>
                    <CardHeader>
                        <CardTitle>Login</CardTitle>
                    </CardHeader>
                    <CardContent>
                        <form onSubmit={(e) => {submit(e, email, pass, loader)}}>
                            <p>Username</p>
                            <input type="text" id="uname" value={email} onChange={(event) => setEmail(event.target.value)} />
                            <p>password</p>
                            <input type="password" id="pword" value={pass} onChange={(event) => setPass(event.target.value)} /> <br/> <br/>
                            <Button type="submit"> 
                                Login
                            </Button> 
                        </form>
                    </CardContent>
                </Card>
            </div>
        </div>
    )
}

const submit = (e: React.FormEvent<HTMLFormElement>, email: string, password: string, nav: () => Promise<void>) => {
    //alert('button clicked! name is: ' + name)
    e.preventDefault()
    axios.post(
        EgressAPI + '/syslogin',
        {                
            password: password,
            email: email
        })
    .then((res)=>{
        sessionStorage.setItem("TOKEN",res.data.sessionToken)
        res.data.profile.forEach((element: any) => {
            sessionStorage.setItem("FIRSTNAME", element.first_name)
            sessionStorage.setItem("LASTNAME", element.last_name)
            sessionStorage.setItem("EMAIL", element.email)
        });
        console.log(sessionStorage.getItem("TOKEN"))
        nav()
    }).catch((error) => {
        console.log(error)
    })
}

export default Login