import './TopBar.css'
import Home from "../icons/home-icon.svg"
import { Link } from 'react-router-dom';

// hovedkomponenten
function TopBar(props) {
    return (  
    <nav>
      <p id = "welcome" >Hello, {props.username} </p>
      <h1 id="title">{props.text}</h1>
      <button onClick={Logout} id = "button_logout">Log out</button>
      <Link to = "/myfridge" id = "button_home">
        <img src = {Home} alt = "Home-Icon" id = "icon_home"/>
      </Link>
    </nav>
    );
}

// funksjonen for å logge ut brukeren
function Logout() {
    sessionStorage.removeItem("sessionToken");
    window.location.href = '/';
}

// eksporterer hovedkomponenten
export default TopBar;