import { useEffect, useState } from 'react'
import { Button } from "@/components/ui/button";
import axios from 'axios';
import { redirect } from 'react-router-dom';
import { Waypoints } from 'lucide-react';
import { Separator } from "@/components/ui/separator"
import { Plus } from 'lucide-react';
import { Input } from "@/components/ui/input"
import { Textarea } from "@/components/ui/textarea"
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
	Accordion,
	AccordionContent,
	AccordionItem,
	AccordionTrigger,
} from "@/components/ui/accordion"
const EgressAPI = import.meta.env.VITE_EAPI_URL
const IngressAPI = import.meta.env.VITE_IAPI_URL

const ManageProcesses= () => {
  // Initialize the state with the defined type
  const [processData, setProcessData] = useState({
    process: [{
      id: 0,
      name: "",
      description: ""
    }]
  });

  const [nameInput, setNameInput] = useState("")
  const [descriptionInput, setDescriptionInput] = useState("")

  const [editIndex, setEditIndex] = useState(-1);
  const [oldtext, setOldText] = useState('');
  const [text, setText] = useState('');
  const [olddesc, setOldDesc] = useState('');
  const [desc, setDesc] = useState('');

  const [textFocus, setTextFocus] = useState(false);
  const [descFocus, setDescFocus] = useState(false);
  const [nameOrNot, setnameOrNot] = useState(false);
  const [search, setSearch] = useState("");
  const [processDropDown, setProcessDropDown] = useState("");
  const [processDropDownId, setProcessDropDownId] = useState(0);

  const [processMachineData, setProcessMachineData] = useState({
    processMachine: [{
      added: false,
      machineEUI: "",
      machineName: "",
    }]
  });

  const [processMachineData2, setProcessMachineData2] = useState({
    processMachine: [{
      added: false,
      machineEUI: "",
      machineName: "",
    }]
  });
    
  const handleEdit = (index: number, value: string, value2: string, value3: boolean) => {
    setEditIndex(index);
    setText(value);
    setOldText(value);
    setDesc(value2);
    setOldDesc(value2);
    setnameOrNot(value3);
  };

  const handleSave = () => {
    setEditIndex(-1);
    editProcess(oldtext, text, olddesc, desc)
  };

  const handleCancelEdit = () => {
    if (!descFocus && !textFocus) {
      setEditIndex(-1);
      setText("");
      setDesc("");
    }
  };

  // Assuming fetchDataProcess is an async function that fetches data
  useEffect(() => {
      fetchDataProcess().then((data) => {
        setProcessData({
          ...processData,
          process: data.map((prcoesses) => ({id: prcoesses.id, name: prcoesses.name, description: prcoesses.description }))
        });
      });
  },[]);

  useEffect(() => {
    fetchDataProcessMachine(processDropDownId).then((data) => {
      setProcessMachineData({
        ...processMachineData,
        processMachine: data.map((processes) => ({
          added: processes.added,
          machineEUI: processes.machineEUI,
          machineName: processes.machineName,
        })),
      });
      setProcessMachineData2({
        ...processMachineData,
        processMachine: data.map((processes) => ({
          added: processes.added,
          machineEUI: processes.machineEUI,
          machineName: processes.machineName,
        })),
      });
    });
  }, [processDropDownId]);

  const [open, setOpen] = useState(false);

  const handleClose = () => {
      setOpen(false);
  };

  
interface Process {
  process: {
      id: number,
      name: string;
      description: string;
  }[];
}
type setProcess = React.Dispatch<React.SetStateAction<{
  process: {
      id: number,
      name: string;
      description: string;
  }[];
}>>
interface ProcessDataForFunction {
  data: Process
  setData: setProcess
}

interface ProcessMachine {
  processMachine: {
    added: boolean;
    machineEUI: string;
    machineName: string;
  }[];
}
interface ProcessMachineInsert {
  processMachine: {
    added: boolean;
    machineEUI: string;
    processId: number;
  }[];
}
interface typeProcessMachineInsert {
  
    added: boolean;
    eui: string;
    processId: number;

}
type setProcessMachine = React.Dispatch<React.SetStateAction<{
  processMachine: {
    added: boolean;
    machineEUI: string;
    machineName: string;
  }[];
}>>
interface ProcessMachineDataForFunction {
  data: ProcessMachine
  setData: setProcessMachine
  setData2: setProcessMachine
  processId: number
}
interface ProcessMachineDataForFunction2 {
  data: ProcessMachine
  data2: ProcessMachine
  processId: number
}

const addProcess = (e: React.FormEvent<HTMLFormElement>, name: string, description: string) =>  {
  e.preventDefault()
  var token: string = ""
  var tokenBool = sessionStorage.getItem("TOKEN")
  if (tokenBool == null) {
      redirect('/')
  } else {
      token = tokenBool
  }
  axios.post(
    IngressAPI + '/new-process',
    {                
      process_name: name,
      description: description,
      sessionToken:token
    }
  ).then((res)=>{
    console.log(res.data)
  }).catch((error) => {
    console.log(error)
    setOpen(true)
  })
}

  const handleSave2 = (data1: any, data2: any, processId: any) => {
    ProcessDataInsertion({ data: data1, data2: data2, processId: processId });
  };
  const ProcessDataInsertion: React.FC<ProcessMachineDataForFunction2> = ({
    data: processMachineData,
    data2: processMachineData2,
    processId,
  }) => {
  var realArray: ProcessMachineInsert[] = [];
  var oldArray: ProcessMachineInsert[] = [];

  processMachineData.processMachine.forEach((item) => {
    realArray.push({
      processMachine: [{
        added: item.added,
        machineEUI: item.machineEUI,
        processId: processId
      }]
    });
  });

  processMachineData2.processMachine.forEach((item) => {
    oldArray.push({
      processMachine: [{
        added: item.added,
        machineEUI: item.machineEUI,
        processId: processId
      }]
    });
  });

  editProcessMachine(realArray, oldArray)
  return null
};

const editProcessMachine = ( realData:  ProcessMachineInsert[], oldData: ProcessMachineInsert[]) =>  {
  var token: string = ""
  var tokenBool = sessionStorage.getItem("TOKEN")
  if (tokenBool == null) {
    redirect('/')
  } else {
    token = tokenBool
  }
  const requestData: typeProcessMachineInsert[] = [];
  realData.forEach((item, index) => {
    if (oldData[index].processMachine) {
      item.processMachine.forEach((item2, index2) => {
        if ( oldData[index].processMachine[index2].added !== item2.added) {
          requestData.push({
            added: item2.added,
            processId: item2.processId,
            eui: item2.machineEUI
          });
        }
      });
    }
  });
  axios.put(
      IngressAPI + '/machineProcess',
      {                
          data: requestData,
          sessionToken:token,
      }
  ).then((res)=>{
    console.log(res.data)
  }).catch((error) => {
    console.log(error)
    setOpen(true)
  })
}

const editProcess = ( oldName: string, newName: string, oldDescription: string, newDescription: string) =>  {
  var token: string = ""
  var tokenBool = sessionStorage.getItem("TOKEN")
  if (tokenBool == null) {
    redirect('/')
  } else {
    token = tokenBool
  }
  axios.put(
    IngressAPI + '/new-process',
      {                
        old_process_name : oldName,
        new_process_name : newName,
        old_description : oldDescription,
        new_description : newDescription,
        sessionToken: token
      }
  ).then((res)=>{
    console.log(res.data)
  }).catch((error) => {
    console.log(error)
    setOpen(true)
  })
}
const deleteProcess = (name: string, description:string) =>  {


  var token: string = ""
  var tokenBool = sessionStorage.getItem("TOKEN")
  if (tokenBool == null) {
    redirect('/')
  } else {
    token = tokenBool
  }
  axios.delete(
    IngressAPI + '/new-process',
      {   
        data:{
          process_name: name,
          description: description,
          sessionToken: token
          }
      }
  ).then((res)=>{
    console.log(res.data)
  }).catch((error) => {
    console.log(error)
    setOpen(true)
  })
}


class Processes {
  id: number = 0;
  name: string = "";
  description: string = "";
  constructor(id: number, name: string, description: string) {
    this.id = id, this.name = name; this.description = description;
  }
}

async function fetchDataProcess(): Promise<Processes[]> {
  var processArrray: Processes[] = []
  await axios.post(EgressAPI + '/process',
  {
    sessionToken: sessionStorage.getItem('TOKEN')
  }).then((res) => {  
    res.data.process.forEach((element: any) => {
      processArrray.push({
        id: element.id,
        name: element.name,
        description: element.description
      })
    });
  }).catch((error) => {
    console.log(error)
    setOpen(true)
  })
  return processArrray 
}

class ProcessesMachine {
  added: boolean = false;
  machineEUI: string = "";
  machineName: string = "";
  constructor(added: boolean, machineEUI: string, machineName: string) {
    this.added = added, this.machineEUI = machineEUI; this.machineName = machineName;
  }
}

async function fetchDataProcessMachine(processId: number): Promise<ProcessesMachine[]> {
  const processArray: ProcessesMachine[] = [];
  try {
    const res = await axios.post(EgressAPI + '/processMachine', {
      process: processId,
      sessionToken: sessionStorage.getItem('TOKEN')
    });
    const { added, machines } = res.data;
    // Iterate over the added array
    added.forEach((add: boolean, index: number) => {
      // Check if machines[index] exists
      if (machines[index]) {
        processArray.push({
          added: add,
          machineEUI: machines[index].eui,
          machineName: machines[index].name,
        });
      }
    });
    return processArray;
  } catch (error) {
    console.log(error);
    return [];
  }
}

  return (
    <div className="flex h-[100%]">
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
      <div className='w-[1100px]'>
        {/* Title and search bar */}
        <div className="w-[100%] h-[40px] flex justify-between content-center" style={{ marginTop: '5px' }}>
					<h1 className="scroll-m-20 text-2xl font-semibold tracking-tight">Processes</h1>
					<Input 
						className="h-[30px] w-[200px]" 
						type="text" 
						value={search} 
						onChange={(event) => setSearch(event.target.value) } 
						placeholder="Search.."
					/>
				</div>

        <Separator className="mb-[10px]" />	

        {/* New process button */}
				<AlertDialog>
					<AlertDialogTrigger> 
						<Button className="w-[60px] h-[30px] mb-[10px] ml-[10px]" variant="default" size="icon" >
							<Waypoints className="h-[20px]" />
							<Plus className="h-[18px]" />
						</Button>
					</AlertDialogTrigger>
					<AlertDialogContent>
            <form onSubmit={(e) => {addProcess(e, nameInput, descriptionInput); }}>
              <Input className="mb-[10px]" type="text" id="uname" value={nameInput} onChange={(event) => setNameInput(event.target.value)} placeholder="Process Name" required/>
              <Textarea id="description" value={descriptionInput} onChange={(event) => setDescriptionInput(event.target.value)} placeholder="Description" required/>
              <br/><br/>
              <AlertDialogFooter>
                <AlertDialogAction>
                  <Button type='submit' onClick={() => {
                    setProcessData(prevState => ({
                      ...prevState,
                      process: [
                        ...prevState.process,
                        {
                          id: 0,
                          name: nameInput,
                          description: descriptionInput
                        }
                      ]
                    }));
                  }}>Submit Process!</Button>
                </AlertDialogAction>
                <AlertDialogCancel>Cancel</AlertDialogCancel>
              </AlertDialogFooter>
            </form>
					</AlertDialogContent>
				</AlertDialog>	

        <Separator />	

        <ul>
          {processData.process.map((process, index) => (
            <li>
              <div className="buildingbar flex items-center">
                <Separator orientation='vertical' className="mr-[5px]"/>
                <div className='flex flex-row'>
                  <Waypoints className="mx-[20px] mt-[2px]" />
                  <strong><h4 className="scroll-m-20 text-xl font-semibold tracking-tight">{process.name}</h4></strong>
                </div> 
                <Separator orientation='vertical' className="mx-[5px]"/>
                <div className="h-[100%] flex items-center justify-around">
                <AlertDialog>
                  <AlertDialogTrigger asChild>
                    <Button className="h-[30px] w-[60px]" variant="outline" onClick={()=>{setText(process.name); setDesc(process.description)}}>Edit</Button>
                  </AlertDialogTrigger>
                  <AlertDialogContent> 
                    <AlertDialogHeader>Edit {process.name}</AlertDialogHeader>
                    <form onSubmit={(e)=>{e.preventDefault();editProcess(process.name, text ,process.description,desc); setText(""); setDesc("");}}>
                      <label>Process Name</label>
                      <Input className="mb-[20px]" type="text" placeholder="Process Name" value={text} onChange={(event) => setText(event.target.value)} required/>
                      <label>Description</label>
                      <Input className="mb-[20px]" type="text" placeholder="Description" value={desc} onChange={(event) => setDesc(event.target.value)} required/>
                      <AlertDialogAction type='submit'>Confirm</AlertDialogAction>
                      <AlertDialogCancel onClick={()=>{setText(""); setDesc("");}}>Cancel</AlertDialogCancel>
                    </form>
                  </AlertDialogContent>
                </AlertDialog>
                <AlertDialog>
                  <AlertDialogTrigger asChild>
                    <Button className="h-[30px] w-[60px]" variant="outline" >Delete</Button>
                  </AlertDialogTrigger>
                  <AlertDialogContent> 
                    <AlertDialogHeader>Are you sure about deleting {process.name}?</AlertDialogHeader>
                    <AlertDialogAction onClick={()=>deleteProcess(process.name, process.description)}>DELETE</AlertDialogAction>
                    <AlertDialogCancel>Cancel</AlertDialogCancel>
                  </AlertDialogContent>
                </AlertDialog>
                </div>
                <Separator orientation='vertical' className="ml-[5px]"/>
              </div>
              <Separator />	
            </li>
          ))}
        </ul>

        <div className="center">
          <AlertDialog>
            <AlertDialogTrigger asChild>
              <Button size="sm" variant="outline" onClick={()=>{}}>Add machine to process</Button>
            </AlertDialogTrigger>
            <AlertDialogContent>
              <AlertDialogHeader>
                <AlertDialogTitle>Add machine to process</AlertDialogTitle>
                <label>Process</label>
                <select onChange={(e) => { const selectedIndex = e.target.selectedIndex; setProcessDropDown(e.target.value); setProcessDropDownId(processData.process[selectedIndex-1].id)}} value={processDropDown}>
                  <option value={""}>Select an option</option>
                  {processData.process.map((pro, index) => (
                    pro.name !== "" ? (
                      <option key={index} value={pro.name}>
                        {pro.name} 
                      </option>
                    ) : null
                  ))}
                </select>
                {processDropDown !== "" && (
                  <div>
                    {processMachineData.processMachine.map((element, index) => (
                      <div key={index}>
                        <input
                          type="checkbox"
                          checked={element.added}
                          onChange={() => {
                            const updatedProcessMachineData = [...processMachineData.processMachine];
                            updatedProcessMachineData[index].added = !element.added;
                            setProcessMachineData({
                              ...processMachineData,
                              processMachine: updatedProcessMachineData
                            });
                          }}
                        />
                        {element.machineEUI}  {element.machineName}
                        <br/>
                      </div>
                    ))}
                  </div>
                )}
              </AlertDialogHeader>
              <AlertDialogFooter>
                <AlertDialogAction type='submit' onClick={()=>{handleSave2(processMachineData, processMachineData2, processDropDownId); console.log(processDropDownId)}}>Save</AlertDialogAction>
                <AlertDialogCancel>Cancel</AlertDialogCancel>
              </AlertDialogFooter>
            </AlertDialogContent>
          </AlertDialog>
        </div>
      </div>
      <Separator className='mx-10' orientation="vertical"/> {/* Needs parent container to be set to 100% height for it to work as it takes on the height of parent container */}
      <div className="w-[400px]">

			</div>
    </div>
  );
};



export default ManageProcesses