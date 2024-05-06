import React, { ChangeEvent } from 'react';
import { useEffect, useState } from 'react'
import { useNavigate } from "react-router-dom";
import { Button } from "@/components/ui/button";
import { redirect } from 'react-router-dom';
import "./adminUserConfig.css"
import axios from 'axios';
import TopBar from '@/components/topbar';
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from "@/components/ui/select"
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
import { Input } from '@/components/ui/input';
import { ScrollArea } from "@/components/ui/scroll-area"
import { z } from "zod"
import { useForm } from "react-hook-form"
import { zodResolver } from "@hookform/resolvers/zod"
import { Scroll } from 'lucide-react';
import { PlusIcon } from 'lucide-react';


const EgressAPI = import.meta.env.VITE_EAPI_URL
const IngressAPI = import.meta.env.VITE_IAPI_URL

const s_schema = z.object({
  email:          z.string().email(),
  firstName:      z.string(),
  lastName:       z.string(),
  permission:     z.coerce.number().nonnegative().safe().max(2),
});



// MAIN COMPONENT
function UserConfig() {
  const [open, setOpen] = useState(false);

  const handleClose = () => {
      setOpen(false);
  };

  const [userData, setUserData] = useState<UserData>({ Users: [] });
  const [search, setSearch] = useState("");
  const [usersToShow, setUsersToShow] = useState<UserData>({ Users: [] });



  var baseAmount = 10
  var multiplier = 1
  var minAmount = baseAmount * (multiplier-1)
  var maxAmount = baseAmount * multiplier


  const m_schema = z.object({
    id:           z.number(),
    email:        z.string(),
    firstName:    z.string(),
    lastName:     z.string(),
    password:     z.string(),
    permission:   z.number(),
  });

  const form = useForm<Users>();

  function onSubmit (values: z.infer<typeof m_schema>) {
    addUser(values.firstName, values.lastName, values.email, values.password, parseInt(values.permission.toString()))
  }

  function onEdit (values: z.infer<typeof m_schema>) {
    editUser(values.id, values.firstName, values.lastName, values.email, values.password, parseInt(values.permission.toString()))
  }
  // Get data when component mounts
  useEffect(() => {
    var userPromise: Promise<UserData> = fetchData();
    userPromise.then(UserDat => {
      setUserData(UserDat)
      setUsersToShow(UserDat)
    })
  }, []);

  // when search term gets updated, redraw and filter list of users
  useEffect(() => {
    if (search != ""){ // filter by search input
      const filteredUsers: UserData = {
        Users: userData.Users.filter(user => user.FirstName.toLowerCase().startsWith(search))
      };
      setUsersToShow(filteredUsers);
    }else{ // if empty search, show everything
      setUsersToShow(userData);
    }
  }, [search])

  /* Set new users to be shown when userData changes */
  useEffect(() => {
    //setUsersToShow(usersData.Users.slice(minAmount, maxAmount));
    setUsersToShow(userData);
  }, [userData, minAmount, maxAmount]);

  // function to filter users
  const filter = (e: React.ChangeEvent<HTMLInputElement>) => {
    setSearch(e.target.value);
  };
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

  
//ADD NEW DATA
const addUser = ( firstName: string, lastName: string, email: string, password: string, permission: number) =>  {

  var token: string = ""
  var tokenBool = sessionStorage.getItem("TOKEN")
  if (tokenBool == null) {
      redirect('/')
  } else {
      token = tokenBool
  }
  axios.post(
    IngressAPI + '/new-user',
    {     
      sessionToken: token, 
      userData: {
        id:         0,
        email:       email,
        first_name:   firstName,
        last_name:    lastName,
        password:    password,
        permission:  permission
      }          
    })
  .then((res)=>{
      console.log(res.data)
  }).catch((error) => {
      console.log(error)
      setOpen(true)
  })
}

///EDIT THE DATA
const editUser = ( id: number, firstName: string, lastName: string, email: string, password: string, permission: number) =>  {
  
  var token: string = ""
  var tokenBool = sessionStorage.getItem("TOKEN")
  if (tokenBool == null) {
    redirect('/')
  } else {
    token = tokenBool
  }
  axios.put(
    IngressAPI + '/new-user',
    {     
      sessionToken: token, 
      userData: {
        id:         id,
        email:       email,
        first_name:   firstName,
        last_name:    lastName,
        password:    password,
        permission:  permission
      }          
    })
  .then((res)=>{
    console.log(res.data)
  }).catch((error) => {
    console.log(error)
    setOpen(true)
  })
}
const deleteUser = ( id: number) =>  {


  var token: string = ""
  var tokenBool = sessionStorage.getItem("TOKEN")
  if (tokenBool == null) {
    redirect('/')
  } else {
    token = tokenBool
  }
  axios.delete(
    IngressAPI + '/new-user',
    {   
      data:{
        sessionToken: token,
        userID: id
      }
    })
  .then((res)=>{
    console.log(res.data)
  }).catch((error) => {
    console.log(error)
    setOpen(true)
  })
}

async function fetchData(): Promise<UserData>{
  var data: UserData = { Users: [] };
  await axios.post(EgressAPI + '/userData',
  {
    sessionToken: sessionStorage.getItem('TOKEN')
  }).then((res) => {
    // extract data from response
    res.data.Users.forEach((user: any) => {
      data.Users.push({id: user.id, FirstName: user.first_name, email: user.email, LastName: user.last_name, permission: user.permission})
    })
    console.log(res.data)
  }).catch((error) => {
    console.log(error)
    setOpen(true)
  })
  return data
}

const permissions: Permission[] = [
  { value: 0, label: 'Admin' },
  { value: 1, label: 'EnøkSuperuser' },
  { value: 2, label: 'Employee' },
];

const translatePermissionValue = (value: number): string => {
  const matchingPermission = permissions.find(permission => permission.value === value);

  return matchingPermission ? matchingPermission.label : 'Unknown';
};

//Classes and interfaces here

//Enum for which operation is used so easy implementation later on
enum Operation {
  add = 0,
  edit = 1,
  delete = 2,
}
// interface for user value from api
interface User {
  id: number,
  email: string,
  FirstName: string,
  LastName: string,
  permission: number
}
// interface for data from api
interface UserData {
  Users: User[]
}

//STUFF FOR SHOWING THE GET DATA AND MORE
interface PermissionDropDownProps {
  selectedValue: number;
  onChange: (value: number) => void;
}

interface Permission {
  value: number;
  label: string;
}
type Users = {
  id: number;
  email: string;
  firstName: string;
  lastName: string;
  password: string;
  permission: number;
}
interface ManageUsersEdit {
  isPopoutOpen: boolean;
  handleClosePopout: () => void;
  handleOpenPopout: () => void;
  handleChangeOperation: (op: number) => void;
  setassignValue: React.Dispatch<React.SetStateAction<number>>;
  usersData: {
    users: {
      id: number;
      email: string;
      FirstName: string;
      LastName: string;
      permission: number;
    }[];
  }
}

//THE STUFF ON THE POPOUT SCREEN
interface ManageUsersProps {
  handleClosePopout: () => void;
  op: Operation;
  assignValue: number;
  usersData: {
    users: {
        id: number;
        email: string;
        FirstName: string;
        LastName: string;
        permission: number;
    }[];
  }
}
    
  // --------------------
  return (
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
        <div className ="leftbar">
        <div className='w-[100%] rounded-md border shadow-md pl-[10px] flex items-center justify-between'>
                    <p>Add User</p>
                    <AlertDialog>
                        <AlertDialogTrigger><Button variant="outline" size="icon"><PlusIcon className='w-[20px]'></PlusIcon></Button></AlertDialogTrigger>
                        <AlertDialogContent>
                            <AlertDialogHeader>
                                <AlertDialogTitle>Add User</AlertDialogTitle>
                            </AlertDialogHeader>

                            <Form {...form}>
                                <form onSubmit={form.handleSubmit(onSubmit)}>
                                    <FormField
                                        control={form.control}
                                        name='email'
                                        render={({ field }) => (
                                            <FormItem>
                                                <FormLabel>Email</FormLabel>
                                                <FormControl>
                                                    <Input placeholder="Email" {...field}/>
                                                </FormControl>
                                            </FormItem> 
                                        )}
                                    />
                                    <FormField
                                        control={form.control}
                                        name='firstName'
                                        render={({ field }) => (
                                            <FormItem>
                                                <FormLabel>First Name</FormLabel>
                                                <FormControl>
                                                    <Input placeholder="First Name" {...field}/>
                                                </FormControl>
                                            </FormItem> 
                                        )}
                                    />
                                    <FormField
                                        control={form.control}
                                        name='lastName'
                                        render={({ field }) => (
                                            <FormItem>
                                                <FormLabel>Last Name</FormLabel>
                                                <FormControl>
                                                    <Input placeholder="Last Name" {...field}/>
                                                </FormControl>
                                            </FormItem> 
                                        )}
                                    />
                                    <FormField
                                        control={form.control}
                                        name='password'
                                        render={({ field }) => (
                                            <FormItem>
                                                <FormLabel>Password</FormLabel>
                                                <FormControl>
                                                    <Input type='password' placeholder="Password" {...field}/>
                                                </FormControl>
                                            </FormItem> 
                                        )}
                                    />
                                    <FormField
                                        control={form.control}
                                        name='permission'
                                        render={({ field }) => (
                                            <FormItem>
                                                <FormLabel>Permission level</FormLabel>
                                                <FormControl>
                                                    <Input placeholder="Permission level" {...field}/>
                                                </FormControl>
                                            </FormItem> 
                                        )}
                                    />
                                    <br/>
                                    <AlertDialogFooter>
                                      <AlertDialogAction type="submit">Submit</AlertDialogAction>
                                      <AlertDialogCancel  onClick={()=>{form.reset()}}>Cancel</AlertDialogCancel>
                                    </AlertDialogFooter>
                                </form>
                            </Form>

                           </AlertDialogContent>
                    </AlertDialog>
                </div>
                <div className = "spacer" />
        </div>
        <div className ="rightbar">
          <div className="w-[250px] h-[70px]">
              <Input type="text" className="outlined-input" value={search} onChange={(event) => setSearch(event.target.value) } placeholder="Search.."/>
          </div>
          <ScrollArea>
            <Table>
              <TableCaption>...</TableCaption>
              <TableHeader>
                <TableRow>
                  <SortableColumnHeader column="id" currentSortedColumn={currentSortedColumn} sortDirection={sortDirection} onClick={handleClick}>ID</SortableColumnHeader>
                  <SortableColumnHeader column="firstName" currentSortedColumn={currentSortedColumn} sortDirection={sortDirection} onClick={handleClick}>First Name</SortableColumnHeader>
                  <SortableColumnHeader column="lastName" currentSortedColumn={currentSortedColumn} sortDirection={sortDirection} onClick={handleClick}>Last Name</SortableColumnHeader>
                  <SortableColumnHeader column="email" currentSortedColumn={currentSortedColumn} sortDirection={sortDirection} onClick={handleClick}>Email</SortableColumnHeader>
                  <SortableColumnHeader column="accessLevel" currentSortedColumn={currentSortedColumn} sortDirection={sortDirection} onClick={handleClick}>Access Level</SortableColumnHeader>
                </TableRow>
              </TableHeader>
              <TableBody>
                {usersToShow.Users.sort((a,b)=>{
                // Ensure currentSortedColumn is defined and not null
                if (!currentSortedColumn) {
                    return 0; 
                }
                var valueA;
                var valueB;

                switch (currentSortedColumn) {
                    case "id":
                        valueA = a.id;
                        valueB = b.id;
                        break;
                    case "firstName":
                        valueA = a.FirstName.toLowerCase();
                        valueB = b.FirstName.toLowerCase();
                        break;
                    case "lastName":
                        valueA = a.LastName.toLowerCase();
                        valueB = b.LastName.toLowerCase();
                        break;
                    case "email":
                        valueA = a.email.toLowerCase();
                        valueB = b.email.toLowerCase();
                        break;
                    case "accessLevel":
                        valueA = a.permission
                        valueB = b.permission
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
            }).map((user, index) => (
                  <TableRow key={index}>
                    <TableCell>{index}</TableCell>
                    <TableCell>{user.FirstName}</TableCell>
                    <TableCell>{user.LastName}</TableCell>
                    <TableCell>{user.email}</TableCell>
                    <TableCell>{user.permission}</TableCell>
                    <div className="center">
                                <AlertDialog>
                                <AlertDialogTrigger asChild>
                                    <Button size="sm" variant="outline">Edit</Button>
                                </AlertDialogTrigger>
                                    <AlertDialogContent>
                                        <AlertDialogTitle>Edit Sensor</AlertDialogTitle>
                                            <Form {...form}>
                                                <form onSubmit={ form.handleSubmit(onEdit)}>
                                                    <FormField
                                                        control={form.control}
                                                        name='id'
                                                        render={({ field }) => (
                                                            <FormItem>
                                                                <FormLabel>ID</FormLabel>
                                                                <FormControl>
                                                                    <Input readOnly defaultValue={user.id} {...field} />
                                                                </FormControl>
                                                            </FormItem> 
                                                        )}
                                                    />
                                                    <FormField
                                                        control={form.control}
                                                        name='firstName'
                                                        render={({ field }) => (
                                                            <FormItem>
                                                                <FormLabel>First Name</FormLabel>
                                                                <FormControl>
                                                                    <Input defaultValue={user.FirstName} {...field} />
                                                                </FormControl>
                                                            </FormItem> 
                                                        )}
                                                    />
                                                    <FormField
                                                        control={form.control}
                                                        name='lastName'
                                                        render={({ field }) => (
                                                            <FormItem>
                                                                <FormLabel>Last Name</FormLabel>
                                                                <FormControl>
                                                                    <Input defaultValue={user.LastName} {...field} />
                                                                </FormControl>
                                                            </FormItem> 
                                                        )}
                                                    />
                                                    
                                                    <br/>
                                                    <FormField
                                                        control={form.control}
                                                        name='email'
                                                        render={({ field }) => (
                                                            <FormItem>
                                                                <FormLabel>Email</FormLabel>
                                                                <FormControl>
                                                                    <Input  defaultValue={user.email} {...field} />
                                                                </FormControl>
                                                            </FormItem> 
                                                        )}
                                                    />
                                                    <br/>
                                                    <FormField
                                                        control={form.control}
                                                        name='password'
                                                        render={({ field }) => (
                                                            <FormItem>
                                                                <FormLabel>New Password</FormLabel>
                                                                <FormControl>
                                                                    <Input type='password' {...field} />
                                                                </FormControl>
                                                            </FormItem> 
                                                        )}
                                                    />
                                                    <br/>
                                                    <FormField
                                                        control={form.control}
                                                        name='permission'
                                                        render={({ field }) => (
                                                            <FormItem>
                                                                <FormLabel>Permission level</FormLabel>
                                                                <FormControl>
                                                                    <Input type="number" defaultValue={user.permission} {...field} />
                                                                </FormControl>
                                                            </FormItem> 
                                                        )}
                                                    /> <br/>
                                                    <AlertDialogFooter>
                                                        <AlertDialogAction type='submit' onClick={()=>{
                                                                if (form.getValues().id === undefined) {
                                                                    form.setValue("id", user.id);
                                                                }
                                                                if (form.getValues().firstName === undefined) {
                                                                    form.setValue("firstName",  user.FirstName);
                                                                }
                                                                if (form.getValues().lastName === undefined) {
                                                                    form.setValue("lastName", user.LastName);
                                                                }
                                                                if (form.getValues().email === undefined) {
                                                                    form.setValue("email",  user.email);
                                                                }
                                                                if (form.getValues().permission === undefined) {
                                                                    form.setValue("permission",  user.permission);
                                                                }
                                                        }}>Done</AlertDialogAction>
                                                        <AlertDialogCancel onClick={()=>{form.reset}}>Cancel</AlertDialogCancel>
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
                                        <AlertDialogDescription>Do you want to delete {user.FirstName + " " + user.LastName}?</AlertDialogDescription>
                                        <AlertDialogAction onClick={() => {deleteUser(user.id); }}>DELETE</AlertDialogAction>
                                        <AlertDialogCancel>Cancel</AlertDialogCancel>
                                    </AlertDialogContent>
                                </AlertDialog>
                            </div>
                  </TableRow>
                ))}
              </TableBody>
            </Table>
          </ScrollArea>
        </div>
      </main>
    </>
  );
};





export default UserConfig;

