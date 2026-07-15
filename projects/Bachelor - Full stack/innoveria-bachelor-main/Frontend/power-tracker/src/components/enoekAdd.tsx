
import React, { useEffect, useState, useRef } from "react";
import TopBar from '@/components/topbar';
//import QRScan from '@/components/qr';
import { redirect } from 'react-router-dom';
import { Button } from "@/components/ui/button";

import axios from 'axios';
import {
    Select,
    SelectContent,
    SelectItem,
    SelectTrigger,
    SelectValue,
} from "@/components/ui/select"
import {
    Card,
    CardContent,
    CardDescription,
    CardFooter,
    CardHeader,
    CardTitle,
} from "@/components/ui/card"
import {
    Table,
    TableBody,
    TableCaption,
    TableCell,
    TableHead,
    TableHeader,
    TableRow,
    SortableColumnHeader,
} from "@/components/ui/table"
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
import {
    Form,
    FormControl,
    FormDescription,
    FormField,
    FormItem,
    FormLabel,
    FormMessage,
} from "@/components/ui/form"
import { ScrollArea } from "@/components/ui/scroll-area"
import { z } from "zod"
import { useForm } from "react-hook-form"
import { zodResolver } from "@hookform/resolvers/zod"
import { PlusIcon } from 'lucide-react';
import { Input } from "@/components/ui/input"
import { machine } from 'os';
import { DatePicker } from '@/components/ui/datepicker'

const EgressAPI = import.meta.env.VITE_EAPI_URL
const IngressAPI = import.meta.env.VITE_IAPI_URL

type EnokDataItem = {
    id: number;
    header: string;
    description: string;
    author: string;
    start_date: Date | undefined;
    end_date: Date | undefined;
    active: boolean;
    process: string;
    approved: boolean | null; // Nullable boolean
  };
  

  interface ManageAddingProps {
    enoek: EnokDataItem[];
    setData: React.Dispatch<React.SetStateAction<{ enoek: EnokDataItem[] }>>;
    search: string;
}
class Processes {
    name: string = "";
     constructor(name: string) {
         this.name = name; 
     }
 }

  


  


const MainComponent: React.FC<ManageAddingProps> = ({ enoek, setData, search }) =>{
    const [title, setTitle] = useState("");
    const [startDate, setStarDate] = React.useState<Date>()
    const [endDate, setEndDate] = React.useState<Date>()
    const [description, setDescription] = useState("");
    const [process, setProcess] = useState("");

    const [processData, setProcessData] = useState({
        process: [{
          name: ""
        }]
    });
    async function fetchDataProcess(): Promise<Processes[]> {
        var processArrray: Processes[] = []
        await axios.post(EgressAPI + '/process',
        {
          sessionToken: sessionStorage.getItem('TOKEN')
        }).then((res) => {  
            res.data.process.forEach((element: any) => {
                processArrray.push({
                  name: element.name
                })
            });
        }).catch((error) => {
          console.log(error)
          setOpen(true)
        })
        return processArrray 
      }
    
    // Assuming fetchDataProcess is an async function that fetches data
    useEffect(() => {
        fetchDataProcess().then((data) => {
          setProcessData({
            ...processData,
            process: data.map((prcoesses) => ({ name: prcoesses.name }))
        });
        });
    }, []);

    const m_schema = z.object({
        id: z.number().int(),
        header: z.string(),
        description: z.string(),
        start_date: z.date().optional(),
        end_date: z.date().optional(),
        process: z.string(),
    });
    
    const form = useForm<EnokDataItem>();
    
    
    function addEnoek(values: z.infer<typeof m_schema>) {
        console.log(values, startDate, endDate)
        var token: string = ""
        var tokenBool = sessionStorage.getItem("TOKEN")
        if (tokenBool == null) {
            redirect('/')
        } else {
            token = tokenBool
        }
        console.log('URL To call: ' + IngressAPI + 'token used' + token)
        axios.post(
            IngressAPI + '/new-enoek',
            {   
                enoek: {
                    header: values.header,
                    description: values.description,
                    start_date: startDate,
                    stop_date:  endDate,
                },             
                sessionToken: token,
                process: values.process
            })
        .then((res)=>{
            console.log(res.data)
        }).catch((error) => {
            console.log(error)
            setOpen(true)
        })
    }
    const [open, setOpen] = useState(false);

    const handleClose = () => {
        setOpen(false);
    };
    

    return (
        <>
        <AlertDialog open={open}>
            <AlertDialogContent>
            <AlertDialogHeader>
                <AlertDialogTitle>Something went wrong</AlertDialogTitle>
            </AlertDialogHeader>
                <div>
                    Please try again or check your connection
                </div>
            <AlertDialogFooter>
                <AlertDialogCancel onClick={handleClose}>Close</AlertDialogCancel>
            </AlertDialogFooter>
            </AlertDialogContent>
        </AlertDialog>
        <div>
        <Form {...form}>
            <form onSubmit={form.handleSubmit(addEnoek)}>
                <FormField
                    control={form.control}
                    name='header'
                    render={({ field }) => (
                        <FormItem>
                            <FormLabel>Header</FormLabel>
                            <FormControl>
                                <Input  placeholder="Header" {...field} />
                            </FormControl>
                        </FormItem> 
                    )}
                />
                <br></br>
                <FormField
                    control={form.control}
                    name='description'
                    render={({ field }) => (
                        <FormItem>
                            <FormLabel>Description</FormLabel>
                            <FormControl>
                                <textarea className={"flex h-10 w-full rounded-md border border-input bg-background px-3 py-2 text-sm ring-offset-background file:border-0 file:bg-transparent file:text-sm file:font-medium placeholder:text-muted-foreground focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-ring focus-visible:ring-offset-2 disabled:cursor-not-allowed disabled:opacity-50"} placeholder="Description" {...field} />
                            </FormControl>
                        </FormItem> 
                    )}
                />
                <br></br>
                <FormField
                    control={form.control}
                    name='start_date'
                    render={({ field }) => (
                        <FormItem>
                            <FormLabel>Start Date</FormLabel>
                            <br></br>
                            <FormControl>
                                <DatePicker dates={startDate} setDate={setStarDate} allowedDate={new Date()} {...field}/>
                            </FormControl>
                        </FormItem> 
                    )}
                />
                <br></br>
                <FormField
                    control={form.control}
                    name='end_date'
                    render={({ field }) => (
                        <FormItem>
                            <FormLabel>Start Date</FormLabel>
                            <br></br>
                            <FormControl>
                                <DatePicker dates={endDate} setDate={setEndDate} allowedDate={new Date("3000 01 01")} {...field}/>
                            </FormControl>
                        </FormItem> 
                    )}
                />
                <FormField
                    control={form.control}
                    name='process'
                    render={({ field }) => (
                        <FormItem>
                            <FormLabel>Process</FormLabel>
                            <Select onValueChange={field.onChange} >
                                <FormControl>
                                    <SelectTrigger>
                                        <SelectValue placeholder="Process" />
                                    </SelectTrigger>
                                </FormControl>
                                <SelectContent>
                                    { processData.process.map((name, index) => {
                                        if (index == -1) return (<></>)
                                        return (
                                            name.name !== "" ? (
                                            <SelectItem key={index} value={name.name}>{name.name}</SelectItem>
                                            ):null
                                        )
                                    })}
                                </SelectContent>
                            </Select>
                        </FormItem> 
                    )}
                />
                <br></br>
                 <Button type='submit'>Done</Button>
                </form>
        </Form>
        {/*
        <form onSubmit={(e) => { addEnoek(e,title,description,startDate,endDate,process); setTitle(""); setStarDate(undefined); setEndDate(undefined); setDescription(""); setProcess("");}}>

        <input type="text" id="uname" value={title} onChange={(event) => setTitle(event.target.value)} placeholder="Title" required/>
        <select name="m_buldings" id="m_building" onChange={(e) => setProcess(e.target.value)} value={process}>
              {processData.process.map((name, index) => (
                   name.name !== "" ? (
                      <option key={index} value={name.name}>
                        {name.name}
                      </option>
                    ) : null
              ))}
            </select>
        <label>
            Date From:
            <DatePicker dates={startDate} setDate={setStarDate} allowedDate={new Date()}/>
        </label> <br/> <br/>
        <label>
            Date To:
            <DatePicker dates={endDate} setDate={setEndDate} allowedDate={new Date("3000 01 01")} />
        </label> <br/> <br/>
        <textarea id="description" value={description} onChange={(event) => setDescription(event.target.value)} placeholder="Description" required/>
        <br/><br/>
        <Button type='submit' onClick={() => {
        setData(prevState => ({
        ...prevState,
        enoek: [
            ...prevState.enoek,
            {
                id: prevState.enoek.length > 0 ? prevState.enoek[prevState.enoek.length - 1].id + 1 : 1,
                header : title,
                description : description,
                author : "YOU",
                start_date : startDate,
                end_date : endDate,
                active : false,
                process : process,
                approved : null,
            }
        ]
        }));

        }}>Submit Enoek!</Button>
        </form>
            */}
        </div></> 
    );
};
  
  export default MainComponent