import { useState } from 'react';
import { useNavigate } from "react-router-dom";
import { Button } from "@/components/ui/button";
import "./login.css";
import axios from 'axios';
import { z } from "zod";
import {
    Card,
    CardContent,
    CardDescription,
    CardFooter,
    CardHeader,
    CardTitle,
} from "@/components/ui/card"
import {
    Form,
    FormControl,
    FormDescription,
    FormField,
    FormItem,
    FormLabel,
    FormMessage,
} from "@/components/ui/form"
import {
    AlertDialog,
    AlertDialogAction,
    AlertDialogCancel,
    AlertDialogContent,
    AlertDialogDescription,
    AlertDialogFooter,
    AlertDialogHeader,
    AlertDialogTitle,
    AlertDialogTrigger,
} from "@/components/ui/alert-dialog"
import { useForm } from "react-hook-form"
import { zodResolver } from "@hookform/resolvers/zod"
import { Input } from "@/components/ui/input"

//var IngressAPI: String = import.meta.env.VITE_IAPI_URL
const EgressAPI = import.meta.env.VITE_EAPI_URL


function Login() {
    const [open, setOpen] = useState(false);

    const handleClose = () => {
        setOpen(false);
    };

    const navigate = useNavigate();

    const loader = async () => {
        navigate("/overview")
    };

    // zod schema for login form
    const l_schema = z.object({
        email:        z.string(),
        password:       z.string()
    });

    const loginForm = useForm<z.infer<typeof l_schema>>({resolver: zodResolver(l_schema)});

    function onSubmit (values: z.infer<typeof l_schema>) {
        axios.post(
            EgressAPI + '/login',
            {                
                password: values.password,
                email: values.email
            })
        .then((res)=>{
            sessionStorage.setItem("TOKEN",res.data.sessionToken)
            res.data.profile.forEach((element: any) => {
                sessionStorage.setItem("USERID", element.id)
                sessionStorage.setItem("FIRSTNAME", element.first_name)
                sessionStorage.setItem("LASTNAME", element.last_name)
                sessionStorage.setItem("EMAIL", element.email)
                sessionStorage.setItem("PERMISSION", element.permission)
            });
            console.log(sessionStorage.getItem("TOKEN"))
            loader()
        }).catch((error) => {
            console.log(error)
            setOpen(true);
        })
    };

    return(
        <div id="loginpage"> {/* !! READ ABOUT USING "zod" https://zod.dev to allow client side validation && needed to make SHADUI form !! */}
        <AlertDialog open={open}>
            <AlertDialogContent>
            <AlertDialogHeader>
                <AlertDialogTitle>Login failed</AlertDialogTitle>
            </AlertDialogHeader>
                <div>
                    Invalid username or password. Please try again.
                </div>
            
            <AlertDialogFooter>
                <AlertDialogCancel onClick={handleClose}>Close</AlertDialogCancel>
            </AlertDialogFooter>
            </AlertDialogContent>
        </AlertDialog>
            <div id="logincard">
                <Card>
                    <CardHeader>
                        <CardTitle>Login</CardTitle>
                    </CardHeader>
                    <CardContent>
                        <Form {...loginForm}>
                            <form onSubmit={loginForm.handleSubmit(onSubmit)}>
                                <FormField
                                    control={loginForm.control}
                                    name="email"
                                    render={({ field }) => (
                                        <FormItem>
                                            <FormLabel>Email</FormLabel>
                                            <FormControl>
                                                <Input placeholder="Email" {...field} />
                                            </FormControl>
                                        </FormItem>
                                    )}
                                />
                                <FormField
                                    control={loginForm.control}
                                    name="password"
                                    render={({ field }) => (
                                        <FormItem>
                                            <FormLabel>Password</FormLabel>
                                            <FormControl>
                                                <Input type='password' placeholder="Password" {...field} />
                                            </FormControl>
                                        </FormItem>
                                    )}
                                /> <br/>
                                <Button type="submit">Login</Button> 
                            </form>
                        </Form>
                    </CardContent>
                </Card>
            </div>
        </div>
    );
}

export default Login