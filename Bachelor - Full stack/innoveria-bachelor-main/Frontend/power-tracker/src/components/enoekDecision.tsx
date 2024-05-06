import { Button } from "@/components/ui/button";
import { DatePicker } from '@/components/ui/datepicker'
import React from "react";
import { useState } from "react";
import axios from 'axios';
import { redirect } from 'react-router-dom';
import { date } from "zod";
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
interface ManageAddingProps {
    enoek: EnokDataItem[];
    setData: React.Dispatch<React.SetStateAction<{ enoek: EnokDataItem[] }>>;
    search: string;
}

const MainComponent: React.FC<ManageAddingProps> = ({ enoek, setData, search}) =>{

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

    const judge = (e: React.MouseEvent<HTMLButtonElement, MouseEvent>, id: number, bool: boolean) =>  {
        e.preventDefault()
    
        var token: string = ""
        var tokenBool = sessionStorage.getItem("TOKEN")
        if (tokenBool == null) {
            redirect('/')
        } else {
            token = tokenBool
        }
        console.log('URL To call: ' + IngressAPI + 'token used' + token)
        axios.put(
            IngressAPI + '/new-enoek',
            {   
                id: id,
                bool: bool,
                sessionToken:token,
            })
        .then((res)=>{
            console.log(res.data)
        }).catch((error) => {
            console.log(error)
            setOpen(true)
        })
    }
    return (
        <>

        <ScrollArea>
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
                <Table>
                    <TableCaption>
                        (づ ◕‿◕ )づ
                        <br />
                        <br />
                    </TableCaption>
                    <TableHeader className="">
                    <TableRow>
                    <SortableColumnHeader
                        column="description"
                        currentSortedColumn={currentSortedColumn}
                        sortDirection={sortDirection}
                        onClick={handleClick}
                    >
                    Description
                    </SortableColumnHeader>
                    <SortableColumnHeader
                        column="author"
                        currentSortedColumn={currentSortedColumn}
                        sortDirection={sortDirection}
                        onClick={handleClick}
                    >
                    Author
                    </SortableColumnHeader>
                    <SortableColumnHeader
                        column="startDate"
                        currentSortedColumn={currentSortedColumn}
                        sortDirection={sortDirection}
                        onClick={handleClick}
                    >
                    Start Date
                    </SortableColumnHeader>
                    <SortableColumnHeader
                        column="endDate"
                        currentSortedColumn={currentSortedColumn}
                        sortDirection={sortDirection}
                        onClick={handleClick}
                    >
                    End Date
                    </SortableColumnHeader>
                    <SortableColumnHeader
                        column="process"
                        currentSortedColumn={currentSortedColumn}
                        sortDirection={sortDirection}
                        onClick={handleClick}
                    >
                    Process
                    </SortableColumnHeader>
                        <TableHead></TableHead>
                    </TableRow>
                </TableHeader>
            {enoek.sort((a,b)=>{
                // Ensure currentSortedColumn is defined and not null
                if (!currentSortedColumn) {
                    return 0; 
                }
                var valueA;
                var valueB;

                switch (currentSortedColumn) {
                    case "description":
                        valueA = a.header.toLowerCase();
                        valueB = b.header.toLowerCase();
                        break;
                    case "author":
                        valueA = a.author.toLowerCase();
                        valueB = b.author.toLowerCase();
                        break;
                    case "startDate":
                        valueA = a.start_date;
                        valueB = b.start_date;
                        break;
                    case "endDate":
                        valueA = a.end_date;
                        valueB = b.end_date;
                        break;
                    case "process":
                        valueA = a.process;
                        valueB = b.process;
                        break;
                    default:
                        return 0; // No sorting if currentSortedColumn is not recognized
                }
                // Compare the values
                if (valueA === undefined || valueB === undefined) {
                    return 0; // No sorting if valueA or valueB is undefined
                } else {
                    // Use ternary operator to decide the comparison direction based on sortDirection
                    const comparison = valueA < valueB ? -1 : valueA > valueB ? 1 : 0;
                    return sortDirection === "asc" ? comparison : -comparison;
                }
            }).map((data, index) => (
                data.start_date != undefined && data.end_date != undefined && data.approved==null&& (data.header.toLowerCase().includes(search.toLowerCase())||data.author.toLowerCase().includes(search.toLowerCase()))?
                <TableRow key={index}>
                    <TableCell><strong>{data.header}</strong> <br></br>{data.description}</TableCell>
                     <TableCell>{data.author}</TableCell>
                     <TableCell>{new Date(data.start_date).toDateString()}</TableCell>
                     <TableCell>{new Date(data.end_date).toDateString()}</TableCell>
                     <TableCell>{data.process}</TableCell>
                     <br></br>
                     <Button size="sm" variant="outline" onClick={(e)=>{judge(e,data.id,true)}}>approve</Button>
                     <Button size="sm" variant="outline" onClick={(e)=>{judge(e,data.id,false)}}>reject</Button>
                </TableRow>
                : null
            ))}
            </Table>
        </ScrollArea>
        </> 
    );
};

  
  export default MainComponent