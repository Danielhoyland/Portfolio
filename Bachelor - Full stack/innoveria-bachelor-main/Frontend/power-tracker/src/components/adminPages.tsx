import '../App.css'
import './topbar.css'
import { useState } from 'react';
import { Button } from "@/components/ui/button"
import { ChevronDown } from 'lucide-react';
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

function AdminPages() {

    const navigate =  useNavigate();
    const [isOpen, setIsOpen] = useState(false)

    function toggle() {
        setIsOpen((isOpen) => !isOpen);
      }

    const logout = async () => {
        return navigate('/');
    };

    return (
        <div id="menubar">
            {/*-------------- MAIN BAR --------------*/}

            <Separator/>
            {/*-------------- NAVIGATION --------------*/}
            <div>
                <div className='navbar'>
                    <NavigationMenu>
                        <NavigationMenuList>
                            <NavigationMenuItem>
                                <NavigationMenuLink className={navigationMenuTriggerStyle()} asChild>
                                    <Link to="/admin1">User configs</Link>
                                </NavigationMenuLink>
                            </NavigationMenuItem>
                            <NavigationMenuItem>
                                <NavigationMenuLink className={navigationMenuTriggerStyle()} asChild>
                                    <Link to="/admin2">System configs</Link>
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

export default AdminPages
