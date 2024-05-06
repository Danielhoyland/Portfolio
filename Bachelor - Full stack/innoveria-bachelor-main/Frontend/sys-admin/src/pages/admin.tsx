import axios from "axios"
import { useState } from "react"
import { redirect } from "react-router-dom"

var IngressAPI: String = import.meta.env.VITE_IAPI_URL

function Adminpage() {
    const [companyName, setCompanyName] = useState("")
    const [email, setEmail] = useState("")
    const [password, setPassword] = useState("")


    return(
        <div>
            <form onSubmit={e => newCompany(e, companyName, email, password)}>
                <h1>Add company</h1>
                <div>
                    <label>
                        Company name
                        <input type='text' value={companyName} onChange={e => setCompanyName(e.target.value)}/>
                    </label>
                    <label>
                        Email for admin user
                        <input type='text' value={email} onChange={e => setEmail(e.target.value)} />
                    </label>
                    <label>
                        Password for admin user
                        <input type='password' value={password} onChange={e => setPassword(e.target.value)} />
                    </label>
                    <input type='submit' value='Submit Company'/>
                </div>
            </form>
        </div>
    )
}

function newCompany(e: React.FormEvent<HTMLFormElement>, CompName: string, email: string, password: string){
    e.preventDefault()
    var token: string = ""
    var tokenBool = sessionStorage.getItem("TOKEN")
    if (tokenBool == null) {
        redirect('/')
    } else {
        token = tokenBool
    }
    axios.post(IngressAPI + "/new-company", {
        sessionToken: token,
        companyName: CompName,
        email: email,
        password: password
    })
    .catch(error => {
        console.log(error)
    })
}

export default Adminpage