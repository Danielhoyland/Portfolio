import '../App.css'
import './topbar.css'
import { useState } from 'react';
import { Button } from "@/components/ui/button"
import { ChevronDown, ZapIcon } from 'lucide-react';
import { ModeToggle } from './mode-toggle';
import { useNavigate } from 'react-router-dom';
import { Link } from 'react-router-dom';
import { Separator } from "@/components/ui/separator"
import {
    NavigationMenu,
    NavigationMenuContent,
    NavigationMenuIndicator,
    NavigationMenuItem,
    NavigationMenuLink,
    NavigationMenuList,
    navigationMenuTriggerStyle,
    NavigationMenuViewport,
} from "@/components/ui/navigation-menu"

function TopBar() {

    const navigate =  useNavigate();
    const [isOpen, setIsOpen] = useState(false)

    function toggle() {
        setIsOpen((isOpen) => !isOpen);
    }

    const logout = async () => {
        sessionStorage.removeItem("TOKEN");
        return navigate('/');
    };

    var Name = sessionStorage.getItem("FIRSTNAME")+ " " + sessionStorage.getItem("LASTNAME")

    return (
        <div id="menubar">
            {/*-------------- MAIN BAR --------------*/}
            <div id="topbar">
                <div id = "topbar-start">
                    <p className = "elem">{Name}</p>
                </div>
                <div className="w-[150px] flex justify-center items-center"><ZapIcon></ZapIcon></div>
                <div id = "topbar-end" className="elem">
                    <ModeToggle></ModeToggle>
                    <Button className = "logout" onClick={logout}>Log Out</Button>
                </div>
            </div>
            <Separator/>
            {/*-------------- NAVIGATION --------------*/}
            <div>
                <div className='navbar'>
                    <NavigationMenu>
                        <NavigationMenuList>
                            <NavigationMenuItem>
                                <NavigationMenuLink className={navigationMenuTriggerStyle()} asChild>
                                    <Link to="/overview">Overview</Link>
                                </NavigationMenuLink>
                            </NavigationMenuItem>
                            <NavigationMenuItem>
                                <NavigationMenuLink className={navigationMenuTriggerStyle()} asChild>
                                    <Link to="/enoek">Enoek</Link>
                                </NavigationMenuLink>
                            </NavigationMenuItem>
                            {sessionStorage.getItem("PERMISSION") === "0" && (
                            <NavigationMenuItem>
                                <NavigationMenuLink className={navigationMenuTriggerStyle()} asChild>
                                    <Link to="/sensors">Sensor Management</Link>
                                </NavigationMenuLink>
                            </NavigationMenuItem>
                            )}
                            {sessionStorage.getItem("PERMISSION") === "0" && (
                            <NavigationMenuItem>
                                <NavigationMenuLink className={navigationMenuTriggerStyle()} asChild>
                                    <Link to="/users">User Configs</Link>
                                </NavigationMenuLink>
                            </NavigationMenuItem>
                            )}
                            {sessionStorage.getItem("PERMISSION") === "0" && (
                            <NavigationMenuItem>
                                <NavigationMenuLink className={navigationMenuTriggerStyle()} asChild>
                                    <Link to="/sys&gate">Gateways and System</Link>
                                </NavigationMenuLink>
                            </NavigationMenuItem>
                            )}
                            <NavigationMenuItem>
                                <NavigationMenuLink className={navigationMenuTriggerStyle()} asChild>
                                    <Link to="/faq">FAQ</Link>
                                </NavigationMenuLink>
                            </NavigationMenuItem>
                        </NavigationMenuList>
                    </NavigationMenu>
                </div>
                <Separator/>
            </div>
            {/*--------------------------------------*/}
        </div>
    )
} 

export default TopBar
