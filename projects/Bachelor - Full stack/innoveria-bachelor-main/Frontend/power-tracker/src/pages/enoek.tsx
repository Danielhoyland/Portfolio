import { useEffect, useState } from 'react'
import { useNavigate } from "react-router-dom";
import { Button } from "@/components/ui/button";
import { redirect } from 'react-router-dom';
import "./adminSysConfig.css"
import axios from 'axios';
import TopBar from '@/components/topbar';
import { Input } from "@/components/ui/input"

import ManageAdding from '../components/enoekAdd'
import AllMeasures from '../components/enoekAllMeasures'
import ActiveMeasures from '../components/enoekAktiv'
import Approved from '../components/enoekApprovedMeasures'
import Rejected from '../components/enoekRejectedMeasures'
import MyMeasures from '../components/enoekMyMeasures'
import Jugde from '../components/enoekDecision'
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


enum Page {
    overview = 0,
    add = 1,
    active = 2,
    approved = 3,
    reject = 4,
    my = 5,
    judge =6
  }

// GET THE DATA
class EnoekClass {
    id: number = 0;
    header: string = "";
    description: string = "";
    author: string = "";
    start_date: Date | undefined = new Date();
    end_date: Date | undefined = new Date();
    active: boolean = false
    process: string = ""
    approved: any = null
    constructor(id: number, header: string, description: string, author: string, start_date: Date | undefined, end_date: Date | undefined, active: boolean, process: string, approved: any) {
        this.id = id;
        this.header = header;
        this.description = description;
        this.author = author;
        this.start_date = start_date;
        this.end_date = end_date;
        this.active = active;
        this.process = process;
        this.approved = approved;
    }
  }
  

const MainComponent= () =>{
    
    const [page, setPage] = useState(Page.overview)
    const [search, setSearch] = useState("");


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
      
      const [enoekData, setEnoekData] = useState<{ enoek: EnokDataItem[] }>({
        enoek: [{
          id: 0,
          header: "",
          description: "",
          author: "",
          start_date: new Date(),
          end_date: new Date(),
          active: false,
          process: "",
          approved: null, // Nullable boolean
        }],
      });
      async function fetchData(): Promise<EnoekClass[]> {
        var enoekData: EnoekClass[] = []
        await axios.post(EgressAPI + '/enoek',
        {
          sessionToken: sessionStorage.getItem('TOKEN')
        }).then((res) => {  
    
          res.data.enoek.forEach((element: any) => {
             enoekData.push({
                id : element.id,
                header : element.header,
                description : element.description,
                author : element.author,
                start_date : element.start_date,
                end_date : element.stop_date,
                active : element.active,
                process : element.process,
                approved : element.approved,
                })
          });
        }).catch((error) => {
          console.log(error)
          setOpen(true)
        })
        
        return enoekData 
      }
    
    
      const deleteEnoek = (e: React.MouseEvent<HTMLButtonElement, MouseEvent>, id: number) =>  {
        e.preventDefault()
      
        var token: string = ""
        var tokenBool = sessionStorage.getItem("TOKEN")
        if (tokenBool == null) {
            redirect('/')
        } else {
            token = tokenBool
        }
        console.log('URL To call: ' + IngressAPI + 'token used' + token)
        axios.delete(
            IngressAPI + '/new-enoek',
            {   
              data:{
                id: id,
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
  
    useEffect(() => {

        fetchData().then((data) => {
          setEnoekData({
            ...enoekData,
            enoek: data.map((enoeks) => ({ id: enoeks.id, header: enoeks.header, description: enoeks.description, author: enoeks.author, start_date: enoeks.start_date, end_date: enoeks.end_date, active: enoeks.active, process: enoeks.process, approved: enoeks.approved }))
        });
        });
    }, []);

    let permissionString = sessionStorage.getItem("PERMISSION");
    let permissionInt 
if (permissionString !== null) {
    // Result is not null, it's safe to assign it to a variable
     permissionInt = parseInt(permissionString, 10);
}
const [open, setOpen] = useState(false);

    const handleClose = () => {
        setOpen(false);
    };

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
              <div className='leftbar'>
                
              {permissionInt!= undefined && permissionInt <= 2 && (
                    <>
                    <Button className="mb-[10px]" variant="ghost" onClick={()=>{setPage(Page.overview); setSearch("");}}><h4 className="scroll-m-20 text-xl font-semibold tracking-tight">All Measures</h4></Button>
                    <br></br>
                    </>
                    
                  )}
                  
                  {permissionInt!= undefined && permissionInt <= 2 && (
                    <>
                 <  Button className="mb-[10px]" variant="ghost" onClick={()=>{setPage(Page.add); setSearch("");}}><h4 className="scroll-m-20 text-xl font-semibold tracking-tight">Add Enoek</h4></Button>
                    <br/>
                    </>
                    
                  )}
                  {permissionInt!= undefined && permissionInt <= 2 && (
                    <>
                    <Button className="mb-[10px]" variant="ghost" onClick={()=>{setPage(Page.active); setSearch("");}}><h4 className="scroll-m-20 text-xl font-semibold tracking-tight">Active Measures</h4></Button>
                    <br/>
                    </>
                    
                  )}
                  {permissionInt!= undefined && permissionInt <= 1 && (
                    <>
                    <Button className="mb-[10px]" variant="ghost" onClick={()=>{setPage(Page.approved); setSearch("");}}><h4 className="scroll-m-20 text-xl font-semibold tracking-tight">Approved Measures</h4></Button>
                    <br/>
                    </>
                    
                  )}
                  {permissionInt!= undefined && permissionInt <= 1 && (
                    <>
                    <Button className="mb-[10px]" variant="ghost" onClick={()=>{setPage(Page.reject); setSearch("");}}><h4 className="scroll-m-20 text-xl font-semibold tracking-tight">Rejected Measures</h4></Button>
                    <br/>
                    </>
                    
                  )}
                  {permissionInt!= undefined && permissionInt <= 2 && (
                    <>
                    <Button className="mb-[10px]" variant="ghost" onClick={()=>{setPage(Page.my); setSearch("");}}><h4 className="scroll-m-20 text-xl font-semibold tracking-tight">My measures</h4></Button>
                    <br/>
                    </>
                    
                  )}
                  {permissionInt!= undefined && permissionInt <= 1 && (
                    <>
                    <Button className="mb-[10px]" variant="ghost" onClick={()=>{setPage(Page.judge); setSearch("");}}><h4 className="scroll-m-20 text-xl font-semibold tracking-tight">Accept/reject measures</h4></Button>
                    <br/>
                    </>
                    
                  )}
                </div>
              
              <div className="rightbar">
                <div className="w-[250px] h-[70px]">
                    <Input type="text" className="outlined-input" value={search} onChange={(event) => setSearch(event.target.value) } placeholder="Search.."/>
                </div>
            {(() => {
              switch (page) {
                case Page.overview:
                  return <AllMeasures enoek={enoekData.enoek} setData={setEnoekData} search={search}></AllMeasures>;
                case Page.add:
                  return <ManageAdding enoek={enoekData.enoek} setData={setEnoekData} search={search}></ManageAdding>;
                case Page.active:
                  return <ActiveMeasures enoek={enoekData.enoek} setData={setEnoekData} search={search}></ActiveMeasures>;
                case Page.approved:
                  return <Approved enoek={enoekData.enoek} setData={setEnoekData} search={search}></Approved>;
                case Page.reject:
                  return <Rejected enoek={enoekData.enoek} setData={setEnoekData} search={search}></Rejected>;
                case Page.my:
                  return <MyMeasures enoek={enoekData.enoek} setData={setEnoekData} search={search}></MyMeasures>;
                case Page.judge:
                  return <Jugde enoek={enoekData.enoek} setData={setEnoekData} search={search}></Jugde>;
                default:
                  return "sad sounds";
              }
            })()}
            
              </div>
            
          
          </main>
        </>
      );
      
};
  
  export default MainComponent