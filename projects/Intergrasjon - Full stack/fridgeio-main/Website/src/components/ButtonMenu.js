import './ButtonMenu.css';
// importering av ikoner
import Carrot from "../icons/carrot-vegetable-icon.svg" 
import Fridge from "../icons/fridge-icon.svg"
import Settings from "../icons/setting-line-icon.svg"
import Book from "../icons/book-icon.svg"

import { Link } from 'react-router-dom';

// Hovedfunksjon for ButtonMenu (Knappmenyen)
function ButtonMenu() {
    return (
      <div class = "buttonmenu">
        <Button name = "Fridge Inventory" img = {Carrot} id = "carrot" link = "/myfridge/inventory" />
        <Button name = "Recipes" img = {Book} id = "icon_book" link = "/myfridge/recipes"/>
        <Button name = "My Fridges" img = {Fridge} id="icon_fridge"/>
        <Button name = "Settings" img = {Settings} id="icon_settings" link = "/myfridge/settings"/>
      </div>
    )
}

// Knappene som bygger opp knappmenyen
function Button(props) {
    return (
      <Link to = {props.link} class = "navbutton">
          <img src={props.img} class="icon" alt="Icon" id={props.id}/>
          {props.name}
      </Link>
    );
}

// Eksportering av Hovedfunksjonen
export default ButtonMenu;