import { useEffect, useState } from 'react'
import { Button } from "@/components/ui/button";
import "../pages/login.css"
import axios from 'axios';
import { redirect } from 'react-router-dom';
import { Input } from "@/components/ui/input"
import { Separator } from "@/components/ui/separator"
import { Plus } from 'lucide-react';
import { Router } from 'lucide-react';
import { Textarea } from "@/components/ui/textarea"
import {
    Table,
    TableBody,
    TableCaption,
    TableCell,
    TableHead,
    TableHeader,
    TableRow,
} from "@/components/ui/table"
import {
    ResizableHandle,
    ResizablePanel,
    ResizablePanelGroup,
} from "@/components/ui/resizable"
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

const EgressAPI = import.meta.env.VITE_EAPI_URL
const IngressAPI = import.meta.env.VITE_IAPI_URL


function ManageGateways() {
    const [euiInput, setEuiInput] = useState("")
    const [nameInput, setNameInput] = useState("")
    const [search, setSearch] = useState("");
    const [gatewayData, setGatewayData] = useState({
        gateway: [{
            eui_gate: "",
            name: ""
        }]
    });
    
    useEffect(() => {
        fetchDataGateway().then((data) => {
            setGatewayData({
                ...gatewayData,
                gateway: data.map((gateway) => ({ eui_gate: gateway.eui_gate, name: gateway.name }))
            });
        });
    }, [setGatewayData]);
    const [open, setOpen] = useState(false);

    const handleClose = () => {
        setOpen(false);
    };

    
const addGateway = ( euiI: string, nameI: string) =>  {

    var token: string = ""
    var tokenBool = sessionStorage.getItem("TOKEN")
    if (tokenBool == null) {
        redirect('/')
    } else {
        token = tokenBool
    }
    console.log('URL To call: ' + IngressAPI + 'token used' + token)
    axios.post(
        IngressAPI + '/add-gateway',
        {                
            name:nameI,
            eui:euiI,
            sessionToken:token
        })
    .then((res)=>{
        console.log(res.data)
    }).catch((error) => {
        console.log(error)
        setOpen(true)
    })
}

const editGateway= ( eui: string, oldEui: string, name: string, oldName: string) =>  {


	var token: string = ""
	var tokenBool = sessionStorage.getItem("TOKEN")
	if (tokenBool == null) {
		redirect('/')
	} else {
		token = tokenBool
	}
	console.log('URL To call: ' + IngressAPI + 'token used' + token)
	axios.put(
		IngressAPI + '/add-gateway',
		{
            name: name,
			oldName: oldName,
			eui: eui,
			oldEui: oldEui,
			sessionToken: token,
		})
	.then((res)=>{
		console.log(res.data)
	}).catch((error) => {
		console.log(error)
        setOpen(true)
	})
}

const deleteGateway = ( eui: string) =>  {

    var token: string = ""
    var tokenBool = sessionStorage.getItem("TOKEN")
    if (tokenBool == null) {
        redirect('/')
    } else {
        token = tokenBool
    }
    console.log('URL To call: ' + IngressAPI + 'token used' + token)
    axios.delete(
        IngressAPI + '/add-gateway',
        {   
        	data:{
                eui: eui,
                sessionToken: token
            }
        })
    .then((res)=>{
        console.log(res.data)
    }).catch((error) => {
        console.log(error)
        setOpen(true)
    })
}

class Gateway {
    eui_gate: string = "";
    name: string = "";
    constructor(eui_gate: string, name:string) {
        this.eui_gate = eui_gate; this.name = name;
    }
}

async function fetchDataGateway(): Promise<Gateway[]> {
    var gateway: Gateway[] = []
    await axios.post(EgressAPI + '/sensorGatewayData',
    {
        sessionToken: sessionStorage.getItem('TOKEN')
    }).then((res) => {  
        res.data.gateway.forEach((element: any) => {
            gateway.push({
            	eui_gate: element.eui_gate,
            	name: element.name
            })
        });
    }).catch((error) => {
    	console.log(error)
        setOpen(true)
    })
    return gateway 
}


    return (
        <ResizablePanelGroup className='flex h-[100%]' direction="horizontal">
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
            <ResizablePanel minSize={72.5} >
                {/* Title and search bar */}
				<div className="w-[100%] h-[40px] flex justify-between content-center" style={{ marginTop: '5px' }}>
					<h1 className="scroll-m-20 text-2xl font-semibold tracking-tight">Gateways</h1>
					<Input 
						className="h-[30px] w-[200px] mr-[10px]" 
						type="text" 
						value={search} 
						onChange={(event) => setSearch(event.target.value) } 
						placeholder="Search.."
					/>
				</div>

                <Separator className="mb-[10px]" />

                {/* New building button */}
				<AlertDialog>
					<AlertDialogTrigger> 
						<Button className="w-[60px] h-[30px] mb-[10px] ml-[10px]" variant="default" size="icon" >
							<Router className="h-[20px]" />
							<Plus className="h-[18px]" />
						</Button>
					</AlertDialogTrigger>
					<AlertDialogContent>
                        <AlertDialogHeader><strong>Add Gateway:</strong></AlertDialogHeader>
                        <form onSubmit={(e) => {addGateway(euiInput, nameInput); setEuiInput(""); setNameInput(""); e.preventDefault()}}>
                            <label>EUI</label>
                            <Input className="mb-[20px]" type="text" value={euiInput} onChange={(event) => setEuiInput(event.target.value)} required/>
                            <label>Name</label>
                            <Input className="mb-[20px]" type="text" value={nameInput} onChange={(event) => setNameInput(event.target.value)} required/>
                            <br/> <br/>
                            <AlertDialogFooter>
                            <AlertDialogAction type='submit'>Submit gateway!</AlertDialogAction>
                            <AlertDialogCancel>Cancel</AlertDialogCancel>
                        </AlertDialogFooter>
                        </form>
					</AlertDialogContent>
				</AlertDialog>		

                <Table className=''>
                    <TableCaption>Gateways</TableCaption>
                    <TableHeader>
                        <TableRow>
                            <TableHead>Name</TableHead>
                            <TableHead>ID</TableHead>
                            <TableHead></TableHead>
                            <TableHead className="text-right"></TableHead>
                        </TableRow>
                    </TableHeader>
                    <TableBody>
                        {gatewayData.gateway.map((gates, index) => (
                            <TableRow>
                                <TableCell className="min-w-[200px] max-w-[300px] scroll-m-20 text-xl font-semibold tracking-tight flex">
                                    <Router className="mx-[20px]" />
                                    <h4 className="scroll-m-20 text-xl font-semibold tracking-tight">{gates.name}</h4>
                                </TableCell>
                                <TableCell className="w-[400px]">{gates.eui_gate}</TableCell>
                                <TableCell className="w-[1fr] flex justify-end">
                                <AlertDialog>
                                    <AlertDialogTrigger asChild>
                                        <Button className="h-[30px] w-[60px]" variant="outline" onClick={()=>{setEuiInput(gates.eui_gate); setNameInput(gates.name)}}>Edit</Button>
                                    </AlertDialogTrigger>
                                    <AlertDialogContent> 
                                        <AlertDialogHeader>Edit {gates.eui_gate} - {gates.name}</AlertDialogHeader>
                                        <form onSubmit={(e)=>{e.preventDefault();editGateway(euiInput,gates.eui_gate, nameInput, gates.name); setEuiInput(""); setNameInput("");}}>
                                        <label>EUI</label>
                                        <Input className="mb-[20px]" type="text" placeholder="EUI" value={euiInput} onChange={(event) => setEuiInput(event.target.value)} required/>
                                        <label>Name</label>
                                        <Input className="mb-[20px]" type="text" placeholder="Name" value={nameInput} onChange={(event) => setNameInput(event.target.value)} required/>
                                        <AlertDialogAction type='submit'>Confirm</AlertDialogAction>
                                        <AlertDialogCancel onClick={()=>{setEuiInput(""); setNameInput("");}}>Cancel</AlertDialogCancel>
                                        </form>
                                    </AlertDialogContent>
                                    </AlertDialog>
                                    <AlertDialog>
                                    <AlertDialogTrigger asChild>
                                        <Button className="h-[30px] w-[60px]" variant="outline" >Delete</Button>
                                    </AlertDialogTrigger>
                                    <AlertDialogContent> 
                                        <AlertDialogHeader>Are you sure about deleting {gates.eui_gate} - {gates.name}?</AlertDialogHeader>
                                        <AlertDialogAction onClick={()=>deleteGateway(gates.eui_gate)}>DELETE</AlertDialogAction>
                                        <AlertDialogCancel>Cancel</AlertDialogCancel>
                                    </AlertDialogContent>
                                </AlertDialog>
                                </TableCell> 
                            </TableRow>
                        ))}
                    </TableBody>
                </Table>
                
            </ResizablePanel>

            <ResizableHandle withHandle className="mx-[10px]" />
            
            <ResizablePanel defaultSize={72.5} className="w-[400px]">

            </ResizablePanel>
        </ResizablePanelGroup>
    )
}




export default ManageGateways