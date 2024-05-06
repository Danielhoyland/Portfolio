import './overview.css' 
//import BarChart from '../components/LineChart';
//import NavBar from "../components/navbar"
import React, { useEffect, useState } from "react";
import LineChart from '../components/LineChart';
import { RefreshCw, Minus, Plus } from 'lucide-react';
import TopBar from '@/components/topbar';
import axios from 'axios';
import {
  Accordion,
  AccordionContent,
  AccordionItem,
  AccordionTrigger,
} from "@/components/ui/accordion"
import { Button } from '@/components/ui/button';
import { DatePicker } from '@/components/ui/datepicker'
import { Input } from '@/components/ui/input';
import { Separator } from "@/components/ui/separator"
import { Checkbox } from "@/components/ui/checkbox"



const IngressAPI: String = import.meta.env.VITE_IAPI_URL
const EgressAPI: String = import.meta.env.VITE_EAPI_URL

const colorList: string[] = [
  'rgb(75, 33, 192)',
  'rgb(75, 192, 192)',
  'rgb(75, 192, 142)',
  'rgb(75, 192, 192)',
  'rgb(241, 212, 192)',
  'rgb(75, 1, 1)',
]

const millisInDays: number = 86400000

function Overview() {

  //list over building data, in a way that linechart can understand
  const [buildingData, setBuildingData] = useState({
    labels: [""],
    datasets: [
      {
        label: "",
        data: [0],
        borderColor: 'rgb(75, 192, 192)',
        tension: 0.1
      }
    ],
  });

  const [rebuild, setRebuild] = useState(false) //"flipping" this will rebuild the page (letting you see changes)

  const [recall, setRecall] = useState(false)   //"flipping" this will re-call the databse, with the updated parameters

  const [startDate, setStartDate] = React.useState<Date>()
  const [endDate, setEndDate] = React.useState<Date>()

  const [buildingsState, setBuildingsState] = useState({
    buildings: [new Building("", new Dataset("", []))],
    processes: [new Process("", "", new Dataset("", []))],
    labels: [new Date()],
    checked: [false]
  })

  const [interval, setInterval] = useState(20)  //the interval between each packet of data
  const [since, setSince] = useState(2)         //how long the first packet of data should be fetched from
  const [start, setStart] = useState(0)
  
  useEffect(() => {
    if (startDate != undefined && endDate != undefined) {
      var sinceStart = new Date().getTime() - startDate.getTime()
      var sinceEnd = new Date().getTime() - endDate.getTime()
      
      sinceStart = Math.floor(sinceStart/86400000)  //millisec to days (86,400,000 milliseconds per day) and turning it to an integer
      sinceEnd = Math.floor(sinceEnd/86400000)
      
      setSince(sinceEnd)
      setStart(sinceStart)
    }
  }, [startDate, endDate])

  useEffect(() => {
    var buildingpromise: Promise<PageData> = fetchData(interval, since, start)
    buildingpromise.then(page => {
      //sets the state of the buildings
      setBuildingsState({
        buildings: page.buildings,
        processes: page.processes,
        labels: page.labels,
        checked: setChecked(page.buildings, page.processes),
      })
      //sets the building data, and creates the datasets for graph
    })
  }, [recall])

  //This effect is supposed to ONLY update if one of the checkboxes are checked/unchecked
  //And thus changing the provided data
  useEffect(() => {
    setBuildingData({
      labels: getLabels(buildingsState.labels),
      datasets: presentData(buildingsState.buildings, buildingsState.processes)
    })
  }, [buildingsState.checked])

  return(
      <>
        <TopBar></TopBar>
        
        <main>

          <div className = "rightbar">
            <div className="h-[100%] w-[100%] relative overflow-auto">
              <LineChart data={buildingData}/>
            </div>
          </div>

          <div className = "leftbar">
            <div>

              {/* GOTTA IMPLEMENT FUNCTIONALITY TO THE 2 BUTTONS TO CHNAGE INOUT */}
              <label className='text-primary text-sm'>Minuites between readings</label>
              <div className='flex'>
                <Input className="w-[280px] h-[40px]" type='number' value={interval} onChange={event => setInterval(parseInt(event.target.value))}></Input>
                <div className="flex flex-col h-[40px] ml-[5px]">
                  <Button className='h-[20px] w-[25px] rounded-none rounded-r-lg' variant="ghost" size="icon" onClick={_ => setInterval(interval+1)}>
                    <Plus className='h-[20px]'></Plus>
                  </Button>
                  <Button className='h-[20px] w-[25px] rounded-none rounded-r-lg' variant="ghost" size="icon" onClick={_ => setInterval(interval-1)}>
                    <Minus className='h-[20px]'></Minus>
                  </Button>
                </div>
              </div>
              
              {/* DATE FROM PICKER */}
              <label className='text-primary text-sm'>Date From</label>
              <div className='flex'>
                <DatePicker width={280} dates={endDate} setDate={setEndDate} allowedDate={startDate} />
                <div className='flex flex-col h-[40px] ml-[5px]'>
                  <Button className='h-[20px] w-[25px] rounded-none rounded-r-lg' variant="ghost" size="icon" onClick={_ => {  //TODO: CLEAN UP THESE LATER ------------------------------- NO REALLY, THEY SUCK ATM
                    var date = new Date;
                    if(startDate !== undefined){
                      date.setTime(startDate.getTime() + 1 * millisInDays);
                      setStartDate(date);
                    }
                    }}>
                    <Plus className='h-[20px]'></Plus>
                  </Button>
                  <Button className='h-[20px] w-[25px] rounded-none rounded-r-lg' variant="ghost" size="icon" onClick={_ => {
                    var date = new Date; var edate = endDate
                    if(startDate !== undefined){
                      if(edate === undefined) edate = new Date(1)
                      if (edate.getTime() < startDate.getTime()){
                      date.setTime(startDate.getTime() - 1 * millisInDays);
                      setStartDate(date);
                    }}}}>
                    <Minus className='h-[22px]'></Minus>
                  </Button>
                </div> 
              </div>

              {/* DATE TO PICKER */}
              <label className='text-primary text-sm'>Date To</label>
              <div className='flex mb-[10px]'>
                <DatePicker width={280} dates={startDate} setDate={setStartDate} allowedDate={new Date()}/>
                <div className='flex flex-col h-[40px] ml-[5px]'>
                  <Button className='h-[20px] w-[25px] rounded-none rounded-r-lg' variant="ghost" size="icon" onClick={e => {  //TODO: CLEAN UP THESE LATER ------------------------------- NO REALLY, THEY SUCK ATM
                  var date = new Date; var sdate = startDate
                  if(endDate !== undefined){
                    if(sdate === undefined) sdate = new Date(1)
                    if (sdate.getTime() > endDate.getTime()){
                    date.setTime(endDate.getTime() + 1 * millisInDays);
                    setEndDate(date);
                  }}}}>
                    <Plus className='h-[20px]'></Plus>
                  </Button>
                <Button className='h-[20px] w-[25px] rounded-none rounded-r-lg' variant="ghost" size="icon" onClick={e => {
                  var date = new Date;
                  if(endDate !== undefined){
                    date.setTime(endDate.getTime() - 1 * millisInDays);
                    setEndDate(date);
                  }
                  }}>
                    <Minus className='h-[22px]'></Minus>
                  </Button>
                </div>
              </div>
              
              {/* DATE HELPER BUTTONS */}
              <div className="flex items-center justify-between mb-[5px]">
                <Button className="w-[100px] h-[30px]" variant="outline" onClick={_ => {
                    var sdate: Date = new Date;
                    var edate: Date = new Date;
                    if(startDate !== undefined && endDate !== undefined){
                      sdate.setTime(startDate.getTime() - 1 * millisInDays); edate.setTime(endDate.getTime() - 1 * millisInDays)
                      setStartDate(sdate);  setEndDate(edate)
                    }}}>-1 day
                </Button>
                <Separator orientation="vertical"></Separator>
                <Button className="w-[100px] h-[30px]" variant="outline" onClick={_ => {
                    var sdate: Date = new Date;
                    var edate: Date = new Date;
                    if(startDate !== undefined && endDate !== undefined){
                      sdate.setTime(startDate.getTime() - 7 * millisInDays); edate.setTime(endDate.getTime() - 7 * millisInDays)
                      setStartDate(sdate);  setEndDate(edate)
                    }}}>-1 week
                </Button>
                </div>
                <Separator className='mb-[5px]'></Separator>
                <div className="flex items-center justify-between">
                  <Button className="w-[100px] h-[30px]" variant="outline" onClick={_ => {setStartDate(new Date()); var d = new Date(); d.setTime(d.getTime() - 1 * millisInDays); setEndDate(d); setInterval(10)}}>Last day</Button>
                  <Separator orientation="vertical"></Separator>
                  <Button className="w-[100px] h-[30px]" variant="outline" onClick={_ => {setStartDate(new Date()); var d = new Date(); d.setTime(d.getTime() - 7 * millisInDays); setEndDate(d); setInterval(30)}}>Last week</Button>
                </div>
              </div>
              <br/>
              <div className="flex items-center align-center justify-center mb-[10px]"><Button className="h-[25px] w-[25px]" variant="outline" size="icon" onClick={_ => setRecall(!recall)}><RefreshCw className='h-[15px] w-[15px]'/></Button></div>
            
            {/*  */}
            <div className ="rounded-md border shadow-md px-[5px]">
              {buildingsState.buildings !== null ?  (
                buildingsState.buildings.map((building, i) => {
                  return (
                    <Accordion type='single' collapsible>
                      <AccordionItem value={i.toString()}>
                      <div id="accordion-bar">
                        <AccordionTrigger className='mx-[5px]'>
                          <label>{building.name}</label>
                        </AccordionTrigger>
                        <Separator orientation='vertical'></Separator>
                        <div className='w-[100%] h-[100%] flex justify-center items-center'>
                          <Checkbox 
                            checked={building.active} 
                            onCheckedChange={_ => {
                              building.active = !building.active; 
                              buildingsState.checked = setChecked(buildingsState.buildings, []); 
                              setRebuild(!rebuild)}}
                          />
                        </div>
                      </div>
                      <AccordionContent>
                      {
                      building.departments.map((department, i) => {
                        return (
                          <div key = {i}>
                              <Accordion type='single' collapsible>
                                <AccordionItem value={i.toString()}>
                                <div id="accordion-bar">
                                  <AccordionTrigger className='mr-[5px] ml-[15px]'>
                                    <label>{department.name}</label>
                                  </AccordionTrigger>
                                  <Separator orientation='vertical'></Separator>
                                  <div className='w-[100%] h-[100%] flex justify-center items-center'>
                                    <Checkbox
                                      checked={department.active} 
                                      onCheckedChange={_ => {
                                        department.active = !department.active; 
                                        buildingsState.checked = setChecked(buildingsState.buildings, []); 
                                        setRebuild(!rebuild)}} 
                                    />
                                  </div>
                                </div>
                                {department.machines.map(machine => {
                                    return(
                                    <AccordionContent className='h-[40px]'>
                                    <div id="accordion-machine">
                                      <div className='mr-[5px] ml-[30px] flex items-center'>
                                        <label>{machine.name}</label>
                                      </div>
                                      <Separator orientation='vertical'></Separator>
                                      <div className='w-[100%] h-[100%] flex justify-center items-center'>
                                        <Checkbox
                                          checked={machine.active} 
                                          onCheckedChange={_ => {
                                            machine.active = !machine.active; 
                                            buildingsState.checked = setChecked(buildingsState.buildings, []); 
                                            setRebuild(!rebuild)}} 
                                        />
                                      </div>
                                    </div>
                                    </AccordionContent>
                                  )
                                })}
                                </AccordionItem>
                              </Accordion>
                          </div>
                        )
                        })}
                      </AccordionContent>
                      </AccordionItem>
                    </Accordion>
                    )
                })) : (
                  <div></div>
                )}
              </div>
              <br/>
              <div className ="rounded-md border shadow-md px-[5px]">
              {buildingsState.processes !== null ? (
                buildingsState.processes.map((process, i) => {
                  return (
                    <Accordion type='single' collapsible>
                      <AccordionItem value={i.toString()}>
                      <div id="accordion-bar">
                        <AccordionTrigger className='mx-[5px]'>
                            <label>{process.name}</label>
                        </AccordionTrigger>
                        <Separator orientation='vertical'></Separator>
                        <div className='w-[100%] h-[100%] flex justify-center items-center'>
                          <Checkbox 
                            checked={process.active} 
                            onCheckedChange={_ => {
                              process.active = !process.active; 
                              buildingsState.checked = setChecked([], buildingsState.processes); 
                              setRebuild(!rebuild)}}
                          />
                        </div>
                      </div> 
                        {process.machines.map(machine => {
                          return(
                          <AccordionContent>
                            <div id="accordion-machine">
                              <div className='mr-[5px] ml-[30px] flex items-center'>
                                <label>{machine.name}</label>
                              </div>
                              <Separator orientation='vertical'></Separator>
                              <div className='w-[100%] h-[100%] flex justify-center items-center'>
                                <Checkbox 
                                  checked={machine.active} 
                                  onCheckedChange={_ => {
                                    machine.active = !machine.active; 
                                    buildingsState.checked = setChecked([], buildingsState.processes); 
                                  setRebuild(!rebuild)}} 
                                />
                              </div>
                            </div>
                          </AccordionContent>
                          )})}
                      </AccordionItem>
                      </Accordion>
                      )
              })) : (
                <div></div>
              )}
            </div>
          </div>

        </main>
      </>
  )
}

async function fetchData(interval: number, since: number, start: number): Promise<PageData> {
  var pageData: PageData = new PageData
  var buildings: Building[] = []
  var processes: Process[] = []
  await axios.post(EgressAPI + '/overview',
  {
    sessionToken: sessionStorage.getItem('TOKEN')
  }, {params: {
    since,
    interval,
    start
  }}).then((res) => {
    //fetch labels, the timestamps in the graph
    pageData.labels = res.data.labels
    //get the buildigns, departments and the individual machines
    res.data.buildings.forEach((building: any) => {
      var dataset: Dataset
      dataset = building.dataset
      dataset.tension = 0.1
      dataset.borderColor = 'rgb(75, 32, 192)'
      var bIdx = buildings.push(new Building(building.name, dataset)) - 1 
      
      building.departments.forEach((department: any) => {
        dataset = department.dataset
        dataset.tension = 0.1
        dataset.borderColor = 'rgb(75, 192, 41)'
        var dIdx = buildings[bIdx].departments.push(new Department(department.name, dataset)) - 1

        department.machines.forEach((machine: any) => {
          dataset = machine.dataset
          dataset.tension = 0.1
          dataset.borderColor = 'rgb(255, 192, 192)'
          buildings[bIdx].departments[dIdx].machines.push(new Machine(machine.eui, machine.name, dataset))
        })
      })
    });
    //get the processes, and their machines
    res.data.processes.forEach((process: any) => {
      var dataset: Dataset
      dataset = process.dataset
      dataset.tension = 0.1
      dataset.borderColor = 'rgb(75, 32, 192)'
      var pIdx = processes.push(new Process(process.name, process.desription, dataset)) - 1 

      process.machines.forEach((machine: any) => {
        dataset = machine.dataset
        dataset.tension = 0.1
        dataset.borderColor = 'rgb(255, 192, 192)'
        processes[pIdx].machines.push(new Machine(machine.eui, machine.name, dataset))
      })
    })
    console.log(res.data.processes)
  }).catch((error) => {
    console.log(error)
  })
  //return at end of function
  pageData.buildings = buildings
  pageData.processes = processes
  return pageData 
}

//Function that creates datasets 
function presentData(buildings: Building[], processes: Process[]): Dataset[] {
  var datasets: Dataset[] = []
  buildings.forEach((building, idx) => {
    if(building.active) {
      datasets.push(buildings[idx].dataset)
    }
    building.departments.forEach(department => {
      if(department.active) {
        datasets.push(department.dataset)
      }
      department.machines.forEach(machine => {
        if (machine.active) {
          datasets.push(machine.dataset)
        }
      })
    })
  })
  processes.forEach(process => {
    if (process.active) {
      datasets.push(process.dataset)
    }
    process.machines.forEach(machine => {
      if (machine.active) {
        datasets.push(machine.dataset)
      }
    })
  })
  return datasets
}

//returns an array of strings, given a building (aka the timestamps). returns empty list on fail
function getLabels(dateList: Date[]): string[] {
  var labels: string[] = []
  if (dateList !== null){
    labels = dateList.map(date => new Date(date).toString().substring(4,24))
  }
  
  return labels
}


//returns a list of booleans
function setChecked(buildings: Building[], processes: Process[]): boolean[] {
  var checkedList: boolean[] = []
  buildings.forEach(building => {
    checkedList.push(building.active)
    building.departments.forEach(department => {
      checkedList.push(department.active)
      department.machines.forEach(machine => {
        checkedList.push(machine.active)
      })
    })
  })
  processes.forEach(process => {
    checkedList.push(process.active)
    process.machines.forEach(machine => {
      checkedList.push(machine.active)
    })
  })
  return checkedList
}

class Building {
  departments: Department[] = []
  name: string = ""
  active: boolean = false
  dataset: Dataset
  
  constructor(name: string, dataset: Dataset) {
    this.name = name
    this.dataset = dataset
  }
}

class Department {
  machines: Machine[] = []
  name: string = "No department"
  active: boolean = false
  dataset: Dataset

  constructor(name: string, dataset: Dataset) {
    this.name = name
    this.machines = []
    this.dataset = dataset
  }
}

class Process {
  name: string = ""
  description: string = ""
  active: boolean = false
  machines: Machine[] = []
  dataset: Dataset

  constructor(name: string, description: string, dataset: Dataset) {
    this.name = name; this.description = description
    this.dataset = dataset
  }
}

class Machine {
  EUI: string =""
  name: string = ""
  active: boolean = false
  dataset: Dataset

  constructor(EUI: string, name: string, dataset: Dataset) {
    this.EUI = EUI; this.name = name
    this.dataset = dataset
  }
}


class Dataset {
    label: string = ""
    data: number[] = []
    borderColor: string = 'rgb(75, 192, 192)'
    tension: number = 0.1

    constructor(label: string, data: number[], borderColor?: string, tension?: number) {
      this.label = label; this.data = data;
      if(borderColor) this.borderColor = borderColor
      if (tension) this.tension = tension
    }
}

class PageData {
  buildings: Building[] = []
  processes: Process[] = []
  labels: Date[] = []
}

export default Overview