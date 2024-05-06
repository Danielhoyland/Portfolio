import { useState } from 'react'
import { useNavigate } from "react-router-dom";
import { Button } from "@/components/ui/button";
import { redirect } from 'react-router-dom';
import "./adminSysConfig.css"
import axios from 'axios';
import TopBar from '@/components/topbar';
import AdminPages from '@/components/adminPages';
import ManageGateways from '../components/manageGateway'
import BuildDepSubpage from '../components/manageBuildDep'
import ManagePrcoesses from '../components/manageProcesses'

const EgressAPI = import.meta.env.VITE_EAPI_URL
const IngressAPI = import.meta.env.VITE_IAPI_URL
//TODO DO THE CALLING OF DATA IN THIS FILE AND NOT WHEN CALLING THE DIFFRENT COMPONENTS
enum subpage {
    buildDep = 0,
    process = 1,
    gateway = 2
}

const MainComponent= () =>{
    const [page, setPage] = useState(subpage.buildDep)
    return (
        <div>
            <TopBar></TopBar>
            <main>
                <div className='leftbar'>
                <Button className="mb-[10px]" variant="ghost" onClick={()=>{setPage(subpage.buildDep);}}>
                    <h4 className="scroll-m-20 text-xl font-semibold tracking-tight">
                        Buildings and Departments
                    </h4>
                </Button>
                <Button className="mb-[10px]" variant="ghost" onClick={()=>{setPage(subpage.process);}}>
                    <h4 className="scroll-m-20 text-xl font-semibold tracking-tight">
                        Processes
                    </h4>
                </Button>
                <br></br>
                <Button className="mb-[10px]" variant="ghost" onClick={()=>{setPage(subpage.gateway);}}>
                    <h4 className="scroll-m-20 text-xl font-semibold tracking-tight">
                        Gateways
                    </h4>
                </Button>
                </div>
                <div className='rightbar'>
                {(() => {
                    switch (page) {
                        case subpage.buildDep:
                            return <BuildDepSubpage />
                        case subpage.process:
                            return <ManagePrcoesses />
                        case subpage.gateway:
                            return <ManageGateways />
                        default:
                            return <BuildDepSubpage />
                    }
                })()}
                </div>
            </main>
        </div>
    );
};

export default MainComponent