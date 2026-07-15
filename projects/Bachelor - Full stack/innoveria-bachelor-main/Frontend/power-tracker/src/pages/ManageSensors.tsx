import './overview.css' 
import React, { useEffect, useState, useRef } from "react";
import TopBar from '@/components/topbar';
//import QRScan from '@/components/qr';
import { redirect } from 'react-router-dom';
import { Button } from "@/components/ui/button";
import "./ManageSensors.css"
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

const EgressAPI = import.meta.env.VITE_EAPI_URL
const IngressAPI = import.meta.env.VITE_IAPI_URL


function ManageSensors() {

    //
    const buttonRef = useRef(null);

    const [sensorData, setSensorData] = useState({
        sensor: [{
          eui: "",
          machine_name: "",
          expected_use: 0,
          machineNr: "",
          building_name: "",
          department_name: "",
          voltage: 0
        }]
    });

    // zod schema for add sensor form
    type AddSensorTypes = {
        eui: string;
        name: string;
        nr: string;
        building: string;
        dept: string;
        euse: number;
        voltage: number;
    }

    const m_schema = z.object({
        eui:        z.string().length(16, {message: "EUI must be exactly 16 characters long."}),
        name:       z.string(),
        nr:         z.string(),
        building:   z.string(),
        dept:       z.string(),
        voltage:    z.number().int(),
        euse:       z.coerce.number().nonnegative().safe(),
    });

    const form = useForm<AddSensorTypes>();

    // function for adding new sensor
    function onSubmit(values: z.infer<typeof m_schema>) {
        
        console.log("WORKS", values)
        
        var token: string = ""
        var tokenBool = sessionStorage.getItem("TOKEN")
        if (tokenBool == null) {
            redirect('/')
        } else {
            token = tokenBool
        }

        axios.post(
            IngressAPI + '/new-hotdrop',
            {                
                name:             values.name,
                eui:              values.eui,
                machine_nr:       values.nr,
                building_name:    values.building,
                department_name:  values.dept,
                expected_use:     parseInt(values.euse.toString()),
                voltage:          parseInt(values.voltage.toString()),
                sessionToken:     token
            })
        .then((res)=>{
            console.log(res.data)
        }).catch((error) => {
            console.log(error)
            setOpen(true)
        })
    }

    // function for editing a sensor
    function editSensor (values: z.infer<typeof m_schema>) {
        console.log(values)
        var token: string = ""
        var tokenBool = sessionStorage.getItem("TOKEN")
        if (tokenBool == null) {
            redirect('/')
        } else {
            token = tokenBool
        }
        var euse;
        if(values.euse == undefined || values.euse == null){
            euse=0
        }else{
            euse=values.euse
        }
        var volt;
        if(values.voltage == undefined || values.voltage == null){
            volt=1
        }else{
            volt=values.voltage
        }
        axios.put(
            IngressAPI + '/new-hotdrop',
            {                
                name:             values.name,
                eui:              values.eui,
                machine_nr:       values.nr,
                building_name:    values.building,
                department_name:  values.dept,
                expected_use:     parseInt(euse.toString()),
                voltage:          parseInt(volt.toString()),
                sessionToken:     token
            })
        .then((res)=>{
            console.log(res.data)
        }).catch((error) => {
            console.log(error)
            setOpen(true)
        })
    }

    class BuildingDep {
        buildingName: string = "";
        departments: string = "";
    
        constructor(buildingName: string, departments: string) {
            this.buildingName = buildingName;
            this.departments = departments;
        }
    }
    
    async function fetchDataBuildingAndDepartment(): Promise<BuildingDep[]> {
    var buiDep: BuildingDep[] = []
    await axios.post(EgressAPI + '/buildDep',
    {
        sessionToken: sessionStorage.getItem('TOKEN')
    }).then((res) => {  
        const buildingDepData = res.data.buildingDep;

        for (const buildingName in buildingDepData) {
            const departments = buildingDepData[buildingName];
            if (Array.isArray(departments)) {
                departments.forEach((department: any) => {
                    buiDep.push(new BuildingDep(buildingName, department.name));
                });
            }
        }
    }).catch((error) => {
        console.log(error)
        setOpen(true)
    })

    return buiDep 
    }

    const [buildDepData, setBuildDepData] = useState({
        buildingDepartmentData: [
           { 
            building: {
                name: ""
            },
            departments: [
                {
                    name: ""
                }
            ]
        }
    
        ]
    });
    
    useEffect(() => {
        fetchDataBuildingAndDepartment().then((buildingDepartments) => {
            setBuildDepData((prevBuildDepData) => {
                const updatedBuildingData = [...prevBuildDepData.buildingDepartmentData];
    
                buildingDepartments.forEach((buildingDepartment) => {
                    // Check if the building name already exists in buildDepData
                    const existingBuildingIndex = updatedBuildingData.findIndex((entry) => entry.building.name === buildingDepartment.buildingName);
    
                    if (existingBuildingIndex !== -1) {
                        // Building name already exists
                        const existingBuilding = updatedBuildingData[existingBuildingIndex];
                        const existingDepartment = existingBuilding.departments.find(department => department.name === buildingDepartment.departments);
    
                        if (!existingDepartment) {
                            // Department doesn't exist for this building, add it
                            existingBuilding.departments.push({
                                name: buildingDepartment.departments
                            });
                        }
                    } else {
                        // Building name doesn't exist, create a new entry
                        updatedBuildingData.push({
                            building: {
                                name: buildingDepartment.buildingName
                            },
                            departments: Array.isArray(buildingDepartment.departments) ? buildingDepartment.departments.map(department => ({ name: department.name })) : [{ name: buildingDepartment.departments }]
                        });
                    }
                });
                return {
                    ...prevBuildDepData,
                    buildingDepartmentData: updatedBuildingData
                };
            });
        });
    }, []);

    class SensorsClass {
        eui: string = "";
        machine_name: string = "";
        expected_use: number = 0;
        machineNr: string = "";
        building_name: string = "";
        department_name: string = "";
        voltage: number = 0;
        constructor(eui: string, machine_name: string, expected_use: number, machineNr: string, building_name: string, department_name: string, voltage: number ) {
            this.eui = eui; this.machine_name = machine_name; this.expected_use = expected_use; this.machineNr = machineNr; this.building_name = building_name; this.department_name = department_name;
            this.voltage = voltage}
    }

    async function fetchDataSensor(): Promise<SensorsClass[]> {
        var sensorArray: SensorsClass[] = []
        await axios.post(EgressAPI + '/sensorGatewayData',
        {
          sessionToken: sessionStorage.getItem('TOKEN')
        }).then((res) => {  
            res.data.sensor.forEach((element: any) => {
                sensorArray.push({
                    eui: element.eui,
                    machine_name: element.machine_name,
                    expected_use: element.expected_use,
                    machineNr: element.machineNr,
                    building_name: element.building_name,
                    department_name: element.department_name,
                    voltage: element.voltage
                })
            });
        }).catch((error) => {
          console.log(error)
          setOpen(true)
        })
        console.log(sensorArray)
        return sensorArray 
    }

    

    const [search, setSearch] = useState("");

    useEffect(() => {
        fetchDataSensor().then((data) => {
          setSensorData({
            ...sensorData,
            sensor: data.map((sensors) => ({ eui: sensors.eui, machine_name: sensors.machine_name, expected_use: sensors.expected_use, machineNr: sensors.machineNr, building_name: sensors.building_name, department_name: sensors.department_name, voltage: sensors.voltage}))
        });
        });
    }, []);


    const deleteSensor= (eui: string) =>  {

        var token: string = ""
        var tokenBool = sessionStorage.getItem("TOKEN")
        if (tokenBool == null) {
            redirect('/')
        } else {
            token = tokenBool
        }
        console.log('URL To call: ' + IngressAPI + 'token used' + token)
        axios.delete(
            IngressAPI + '/new-hotdrop',
            {   
              data:{
                eui: eui,
                sessionToken: token,
                }
            })
        .then((res)=>{
            console.log(res.data)
        }).catch((error) => {
            console.log(error)
            setOpen(true)
        })
      }
      

    type BuildDepData = {
        buildingDepartmentData: {
            building: {
                name: string;
            };
            departments: {
                name: string;
            }[];
        }[];
    };

    const watchBuilding = form.watch("building", "null");
    React.useEffect(() => {
        const subscription = form.watch((value, { name, type }) =>
            console.log("hi")
        )
        return () => subscription.unsubscribe()
    }, [form.watch])

    
    const [currentSortedColumn, setCurrentSortedColumn] = useState<string | null>(null);
    const [sortDirection, setSortDirection] = useState<'asc' | 'desc' | null>(null);

    const handleClick = (column: string) => {
        if (currentSortedColumn === column) {
        // Toggle sort direction if clicking on the same column
        setSortDirection(sortDirection === 'asc' ? 'desc' : 'asc');
        } else {
        // Set new sorted column and reset sort direction
        setCurrentSortedColumn(column);
        setSortDirection('asc');
        }
    };
    const [open, setOpen] = useState(false);

    const handleClose = () => {
        setOpen(false);
    };

    return(
        <>
        <TopBar></TopBar>
        <main>
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
            <div className='leftbar'>
                <div className='w-[100%] rounded-md border shadow-md pl-[10px] flex items-center justify-between'>
                    <p>Add Sensor</p>
                    <AlertDialog>
                        <AlertDialogTrigger><Button variant="outline" size="icon"><PlusIcon className='w-[20px]'></PlusIcon></Button></AlertDialogTrigger>
                        <AlertDialogContent>
                            <AlertDialogHeader>
                                <AlertDialogTitle>Add Sensor</AlertDialogTitle>
                            </AlertDialogHeader>

                            <Form {...form}>
                                <form onSubmit={form.handleSubmit(onSubmit)}>
                                    <FormField
                                        control={form.control}
                                        name='eui'
                                        render={({ field }) => (
                                            <FormItem>
                                                <FormLabel>EUI</FormLabel>
                                                <FormControl>
                                                    <Input placeholder="EUI" {...field}/>
                                                </FormControl>
                                                <FormDescription>
                                                    The id code found under "MAC" on sensor
                                                </FormDescription>
                                            </FormItem> 
                                        )}
                                    />
                                    <FormField
                                        control={form.control}
                                        name='name'
                                        render={({ field }) => (
                                            <FormItem>
                                                <FormLabel>Name</FormLabel>
                                                <FormControl>
                                                    <Input placeholder="Name" {...field}/>
                                                </FormControl>
                                            </FormItem> 
                                        )}
                                    />
                                    <FormField
                                        control={form.control}
                                        name='nr'
                                        render={({ field }) => (
                                            <FormItem>
                                                <FormLabel>Nr</FormLabel>
                                                <FormControl>
                                                    <Input placeholder="Nr" {...field}/>
                                                </FormControl>
                                                <FormDescription>
                                                    Custom machine nr
                                                </FormDescription>
                                            </FormItem> 
                                        )}
                                    />
                                    <FormField
                                        control={form.control}
                                        name='building'
                                        render={({ field }) => (
                                            <FormItem>
                                                <FormLabel>Building</FormLabel>
                                                <Select onValueChange={field.onChange} >
                                                    <FormControl>
                                                        <SelectTrigger>
                                                            <SelectValue placeholder="Building" />
                                                        </SelectTrigger>
                                                    </FormControl>
                                                    <SelectContent>
                                                        { buildDepData.buildingDepartmentData.map((building, index) => {
                                                            if (index == 0) return (<></>)
                                                            return (
                                                                <SelectItem key={index} value={building.building.name}>{building.building.name}</SelectItem>
                                                            )
                                                        })}
                                                    </SelectContent>
                                                </Select>
                                            </FormItem> 
                                        )}
                                    />
                                    { /* If a building is selected, render department picker */ }
                                    { (watchBuilding != "") ? (
                                        <FormField
                                            control={form.control}
                                            name='dept'
                                            render={({ field }) => (
                                                <FormItem>
                                                    <FormLabel>Department</FormLabel>
                                                    <Select onValueChange={field.onChange} defaultValue={field.value}>
                                                        <FormControl>
                                                            <SelectTrigger>
                                                                <SelectValue placeholder="Department" />
                                                            </SelectTrigger>
                                                        </FormControl>
                                                        <SelectContent>
                                                        <SelectItem key={1} value={" "}> </SelectItem>
                                                            { buildDepData.buildingDepartmentData.map((building, index) => {
                                                                if (index == 0) return (<></>)
                                                                if (building.building.name === watchBuilding) return (
                                                                    <div key={index}>
                                                                        { building.departments.map((dept, index) => {
                                                                            if (index == -1) return (<></>)
                                                                            else return (
                                                                                <SelectItem key={index} value={dept.name}>{dept.name}</SelectItem>
                                                                            )
                                                                        })}
                                                                    </div>
                                                                )
                                                            })}
                                                        </SelectContent>
                                                    </Select>
                                                </FormItem> 
                                            )}
                                        />
                                    ) : (
                                        <></>
                                    )}
                                    <FormField
                                        control={form.control}
                                        name='euse'
                                        render={({ field }) => (
                                            <FormItem>
                                                <FormLabel>Expected power usage</FormLabel>
                                                <FormControl>
                                                    <Input type="number" placeholder="0"  {...field}/>
                                                </FormControl>
                                            </FormItem> 
                                        )}
                                    />
                                    <FormField
                                        control={form.control}
                                        name='voltage'
                                        render={({ field }) => (
                                            <FormItem>
                                                <FormLabel>Voltage</FormLabel>
                                                <FormControl>
                                                    <Input type="number" placeholder="0" {...field} />
                                                </FormControl>
                                            </FormItem> 
                                        )}
                                    /> <br/>
                                    <AlertDialogFooter>
                                        <AlertDialogAction type='submit'>Done</AlertDialogAction>
                                        <AlertDialogCancel onClick={()=>{form.reset()}}>Cancel</AlertDialogCancel>
                                    </AlertDialogFooter>
                                </form>
                            </Form>

                            
                            </AlertDialogContent>
                    </AlertDialog>
                </div>
                <div className = "spacer" />
            </div>
            <div className="rightbar">
                <div className="w-[250px] h-[70px]">
                    <Input type="text" className="outlined-input" value={search} onChange={(event) => setSearch(event.target.value) } placeholder="Search.."/>
                </div>
                <ScrollArea>
                <Table>
                    <TableCaption>
                        (づ ◕‿◕ )づ
                        <br />
                        <br />
                    </TableCaption>
                    <TableHeader className="">
                    <TableRow>
                    <SortableColumnHeader
                        column="id"
                        currentSortedColumn={currentSortedColumn}
                        sortDirection={sortDirection}
                        onClick={handleClick}
                    >
                    ID
                    </SortableColumnHeader>
                    <SortableColumnHeader
                        column="name"
                        currentSortedColumn={currentSortedColumn}
                        sortDirection={sortDirection}
                        onClick={handleClick}
                    >
                    Name
                    </SortableColumnHeader>
                    <SortableColumnHeader
                        column="nr"
                        currentSortedColumn={currentSortedColumn}
                        sortDirection={sortDirection}
                        onClick={handleClick}
                    >
                    Nr
                    </SortableColumnHeader>
                    <SortableColumnHeader
                        column="building"
                        currentSortedColumn={currentSortedColumn}
                        sortDirection={sortDirection}
                        onClick={handleClick}
                    >
                    Building
                    </SortableColumnHeader>
                    <SortableColumnHeader
                        column="department"
                        currentSortedColumn={currentSortedColumn}
                        sortDirection={sortDirection}
                        onClick={handleClick}
                    >
                    Department
                    </SortableColumnHeader>
                    <SortableColumnHeader
                        column="expectedUsage"
                        currentSortedColumn={currentSortedColumn}
                        sortDirection={sortDirection}
                        onClick={handleClick}
                    >
                    Expected Usage
                    </SortableColumnHeader>
                    <SortableColumnHeader
                        column="voltage"
                        currentSortedColumn={currentSortedColumn}
                        sortDirection={sortDirection}
                        onClick={handleClick}
                    >
                    Voltage
                    </SortableColumnHeader>
                        <TableHead></TableHead>
                    </TableRow>
                </TableHeader>

                    <TableBody>
                    {sensorData.sensor.sort((a,b)=>{
                // Ensure currentSortedColumn is defined and not null
                if (!currentSortedColumn) {
                    return 0; 
                }
                var valueA;
                var valueB;

                switch (currentSortedColumn) {
                    case "id":
                        valueA = a.eui.toLowerCase();
                        valueB = b.eui.toLowerCase();
                        break;
                    case "name":
                        valueA = a.machine_name.toLowerCase();
                        valueB = b.machine_name.toLowerCase();
                        break;
                    case "nr":
                        valueA = a.machineNr;
                        valueB = b.machineNr;
                        break;
                    case "building":
                        valueA = a.building_name.toLowerCase();
                        valueB = b.building_name.toLowerCase();
                        break;
                    case "department":
                        valueA = a.department_name ? a.department_name.toLowerCase() : null;
                        valueB = b.department_name ? b.department_name.toLowerCase() : null;
                        break;
                    case "expectedUsage":
                        valueA = a.expected_use;
                        valueB = b.expected_use;
                        break;
                    case "voltage":
                        valueA = a.voltage;
                        valueB = b.voltage;
                        break;
                    default:
                        return 0; // No sorting if currentSortedColumn is not recognized
                }
                // Compare the values
                if (valueA === null || valueB === null) {
                    return 0; // No sorting if valueA or valueB is undefined
                } else {
                    // Use ternary operator to decide the comparison direction based on sortDirection
                    const comparison = valueA < valueB ? -1 : valueA > valueB ? 1 : 0;
                    return sortDirection === "asc" ? comparison : -comparison;
                }
            }).map((machine, index) => {
                    return (
                        machine.eui.toLowerCase().includes(search.toLowerCase())||machine.machine_name.toLowerCase().includes(search.toLowerCase())?
                        <TableRow  key={machine.eui}>
                            <TableCell>{machine.eui}</TableCell>
                            <TableCell>{machine.machine_name}</TableCell>
                            <TableCell>{machine.machineNr}</TableCell>
                            <TableCell>{machine.building_name}</TableCell>
                            <TableCell>{machine.department_name}</TableCell>
                            <TableCell>{machine.expected_use}</TableCell>
                            <TableCell>{machine.voltage}</TableCell>
                            <div className="center">
                                <AlertDialog>
                                <AlertDialogTrigger asChild>
                                    <Button size="sm" variant="outline">Edit</Button>
                                </AlertDialogTrigger>
                                    <AlertDialogContent>
                                        <AlertDialogTitle>Edit Sensor</AlertDialogTitle>
                                            <Form {...form}>
                                                <form onSubmit={ form.handleSubmit(editSensor)}>
                                                    <FormField
                                                        control={form.control}
                                                        name='eui'
                                                        render={({ field }) => (
                                                            <FormItem>
                                                                <FormLabel>EUI</FormLabel>
                                                                <FormControl>
                                                                    <Input readOnly defaultValue={machine.eui} {...field} />
                                                                </FormControl>
                                                            </FormItem> 
                                                        )}
                                                    />
                                                    <FormField
                                                        control={form.control}
                                                        name='name'
                                                        render={({ field }) => (
                                                            <FormItem>
                                                                <FormLabel>Name</FormLabel>
                                                                <FormControl>
                                                                    <Input defaultValue={machine.machine_name} {...field} />
                                                                </FormControl>
                                                            </FormItem> 
                                                        )}
                                                    />
                                                    <FormField
                                                        control={form.control}
                                                        name='nr'
                                                        render={({ field }) => (
                                                            <FormItem>
                                                                <FormLabel>Nr</FormLabel>
                                                                <FormControl>
                                                                    <Input defaultValue={machine.machineNr} {...field} />
                                                                </FormControl>
                                                            </FormItem> 
                                                        )}
                                                    />
                                                    <FormField
                                                        control={form.control}
                                                        name='building'
                                                        render={({ field }) => (
                                                            <FormItem>
                                                                <FormLabel>Building</FormLabel>
                                                                <Select onValueChange={field.onChange} defaultValue={machine.building_name}>
                                                                    <FormControl>
                                                                        <SelectTrigger>
                                                                            <SelectValue  />
                                                                        </SelectTrigger>
                                                                    </FormControl>
                                                                    <SelectContent>
                                                                        { buildDepData.buildingDepartmentData.map((building, index) => {
                                                                            if (index == 0) return (<></>)
                                                                            return (
                                                                                <SelectItem key={index} value={building.building.name}>{building.building.name}</SelectItem>
                                                                            )
                                                                        })}
                                                                    </SelectContent>
                                                                </Select>
                                                            </FormItem> 
                                                        )}
                                                    />
                                                    <FormField
                                                        control={form.control}
                                                        name='dept'
                                                        render={({ field }) => (
                                                            <FormItem>
                                                                <FormLabel>Department</FormLabel>
                                                                <Select onValueChange={field.onChange} defaultValue={machine.department_name}>
                                                                    <FormControl>
                                                                        <SelectTrigger>
                                                                            <SelectValue  />
                                                                        </SelectTrigger>
                                                                    </FormControl>
                                                                    <FormDescription>
                                                                        {"Please select building first to change department  : )"}
                                                                    </FormDescription>
                                                                    <SelectContent>
                                                                    <SelectItem key={1} value={" "}></SelectItem>
                                                                        { buildDepData.buildingDepartmentData.map((building, index) => {
                                                                            if (index == 0) return (<></>)
                                                                            if (building.building.name === watchBuilding || building.building.name ===machine.building_name) return (
                                                                                <div key={index}>
                                                                                    { building.departments.map((dept, index) => {
                                                                                        if (index == -1) return (<></>)
                                                                                        else return (
                                                                                            <SelectItem key={index} value={dept.name}>{dept.name}</SelectItem>
                                                                                        )
                                                                                    })}
                                                                                </div>
                                                                            )
                                                                        })}
                                                                    </SelectContent>
                                                                </Select>
                                                            </FormItem> 
                                                        )}
                                                    />
                                                    <br/>
                                                    <FormField
                                                        control={form.control}
                                                        name='euse'
                                                        render={({ field }) => (
                                                            <FormItem>
                                                                <FormLabel>Expected power usage</FormLabel>
                                                                <FormControl>
                                                                    <Input type="number" defaultValue={machine.expected_use} {...field} />
                                                                </FormControl>
                                                            </FormItem> 
                                                        )}
                                                    />
                                                    <br/>
                                                    <FormField
                                                        control={form.control}
                                                        name='voltage'
                                                        render={({ field }) => (
                                                            <FormItem>
                                                                <FormLabel>Voltage</FormLabel>
                                                                <FormControl>
                                                                    <Input type="number" defaultValue={machine.voltage} {...field} />
                                                                </FormControl>
                                                            </FormItem> 
                                                        )}
                                                    /> <br/>
                                                    <AlertDialogFooter>
                                                        <AlertDialogAction type='submit' onClick={()=>{
                                                                if (form.getValues().eui === undefined) {
                                                                    form.setValue("eui", machine.eui);
                                                                }
                                                                if (form.getValues().name === undefined) {
                                                                    form.setValue("name", machine.machine_name);
                                                                }
                                                                if (form.getValues().nr === undefined) {
                                                                    form.setValue("nr", machine.machineNr);
                                                                }
                                                                if (form.getValues().building === undefined) {
                                                                    form.setValue("building", machine.building_name);
                                                                }
                                                                if (form.getValues().dept === undefined) {
                                                                    form.setValue("dept", machine.department_name);
                                                                }
                                                                if (form.getValues().euse === undefined) {
                                                                    form.setValue("euse", machine.expected_use);
                                                                }
                                                                if (form.getValues().voltage === undefined) {
                                                                    form.setValue("voltage", machine.voltage);
                                                                }}
                                                        }>Done</AlertDialogAction>
                                                        <AlertDialogCancel>Cancel</AlertDialogCancel>
                                                    </AlertDialogFooter>
                                                </form>
                                            </Form>
                                    </AlertDialogContent>
                                </AlertDialog>
                                <AlertDialog>
                                <AlertDialogTrigger asChild>
                                    <Button size="sm" variant="outline">Delete</Button>
                                </AlertDialogTrigger>
                                    <AlertDialogContent>
                                        <AlertDialogTitle>Delete Sensor?</AlertDialogTitle>
                                        <AlertDialogDescription>Do you want to delete {machine.eui} - {machine.machine_name} ?</AlertDialogDescription>
                                        <AlertDialogAction onClick={() => {deleteSensor(machine.eui); }}>DELETE</AlertDialogAction>
                                        <AlertDialogCancel onClick={()=>{form.reset()}}>Cancel</AlertDialogCancel>
                                    </AlertDialogContent>
                                </AlertDialog>
                            </div>

                        </TableRow>:null
                    )
                    })}
                    </TableBody>
                </Table>
                </ScrollArea> 
            </div>
        </main>
        </>
    )
}

export default ManageSensors