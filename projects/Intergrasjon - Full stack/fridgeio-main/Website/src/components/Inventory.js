import React, { Component } from 'react'; //importing react for react components
import axios from 'axios';
import './Inventory.css'
import TopBar from './TopBar.js'
import ButtonMenu from './ButtonMenu'
// importering av ikoner
import Cross from "../icons/cross.svg"
import Star_line from "../icons/favorite-line.svg"
import Star_full from "../icons/favorite-full.svg"

const favoriteUrl = process.env.REACT_APP_API_URL +  "/setFavorite"

// Hovedkomponenten for siden
class Inventory extends Component {
  constructor(props){
    super(props)

    this.state ={
      isTabOpen: false,
      user: {
        username:"",
        email: ""
      },
      products: [],
      userlists: []
    }
  }

  // funksjonen for å endre boolen isTabOpen til motsatt
  toggleTab = () => {
    this.setState((prevState) => ({
      isTabOpen: !prevState.isTabOpen,
    }));
  };

  // kjører når komponenten kjører, redirekter til login page 
  componentDidMount() {
    console.log("Data in Home: ", this.props.data)
    const sessionToken = sessionStorage.getItem('sessionToken');
    if (sessionToken) {
      this.props.getData();
    } else {
      window.location.href = '/login';
    }
  }

  // funksjonen for å endre favoritt på produkt i databasen, så refresh pagen
  favorite = (id) => {
    console.log("Favoriting: ", id);
    axios.post(favoriteUrl, {
      token: sessionStorage.getItem('sessionToken'),
      id: id
    })
    .then((res)=>{
      console.log("")
      this.props.getData();
    })
    .catch((err) => {
      console.error("Error favoriting product:", err);
    })
  }

  // sletting av produkt fra databasen, refresh siden
  deleteProduct = (id) => {
    const requestData = {
      user: {
        sessiontoken: sessionStorage.getItem("sessionToken")
      },
      product: {
        id: id
      }
    };
    console.log("Deleting....", id)
    console.log({
      user: {
        sessiontoken: sessionStorage.getItem("sessionToken")
      },
      product:{
        id: id
      }
    })
    axios.delete(this.props.addproducturl, {
      data: requestData
    })
    .then((res)=>{
      this.props.getData();
    })
    .catch((err) => {
      console.error("Error deleting product:", err);
    })
  }

  // hoved html'en
  render () {
    return (
      <main id = "page_inventory">
        <div class = "sidebar" id = "sidebar_right"/>
        <div class = "sidebar" id = "sidebar_left"/>
        <TopBar username = {this.props.data.user.username} text = "Inventory"/>
        <ProductList data={this.props.data} deleteProduct={this.deleteProduct} favorite={this.favorite}/>
        {/* Knappen for å åpne/lukke brukerinputformen */}
        <button id = "button_toggle" onClick={this.toggleTab}>
          {this.state.isTabOpen ? 'Close Form' : 'Add Products'}
        </button>
        {/* sjekker om formen skal være åpen eller ikke */}
        {this.state.isTabOpen && (
          <form id = "addproduct" class = "hidden" onSubmit={e => this.props.addProduct(e)}>
            <input className = "input" type = "text" name = "productname" 
              onChange = {this.props.onChange} 
              value={this.props.data.product.productname} 
              placeholder='Name'/>
            <input className = "input" type = "text" name = "measure" 
              onChange = {this.props.onChange} 
              value={this.props.data.product.measure} 
              placeholder='Form of Measurement'/>
            <input className = "input" type = "text" name = "type" 
              onChange = {this.props.onChange} 
              value={this.props.data.product.type} 
              placeholder='Type'/>
            <input className = "input" type = "date" name = "expiration"
              onChange = {this.props.onChange}
              value={this.props.data.product.expiration}
              placeholder='Expiration'/>
            <input className = "input" type = "number" name = "amount"
              onChange = {this.props.onChange}
              value={this.props.data.product.amount}
              placeholder='Amount'/>
            <button type = "submit" id = "submit" >Submit</button>
          </form>
        )}
        <ButtonMenu/>
      </main>
    );
  }
}

// listen over produkter
class ProductList extends Component {
  render() {
    const { data } = this.props;
    // hvis produkter er tomme
    if (data.products == null){
     return(<div></div>);
    }else{
      return (
        <div id="productbox">
          {data.products.map((prod, i) => (
              <Product 
                prod={prod} 
                ulist={data.userlists[i]} 
                id={i} 
                deleteProduct={this.props.deleteProduct}
                favorite={this.props.favorite}
                active={true}
                type = "product"/>
            ))}
        </div>       
      );
    }  
  }
}

// funksjonen for selve produktene
export class Product extends Component {
  render() {
    // prod = input strukten
    // id = id for produktet
    // ulist = userlists
    // active = om produktet skal ha knapper for favoritt/sletting
    const { prod, id, ulist, active } = this.props;
    return (
      <div className = "product" id={id}>
        <div className="product-layout">

        <div className="product_item">
          <h3>{prod.productname}</h3>
        </div>

        {active ? (
            <button className="button_favorite" onClick={() => this.props.favorite(prod.id)}> 
              {ulist.favorite ? (
                <img src={Star_full} id="star" alt="Favorited" />
              ) : (
                <img src={Star_line} id="star" alt="Not Favorited" />
              )}
            </button>
          ) : (
          <button className="button_favorite">
            {ulist.favorite ? (
                <img src={Star_full} id="star" alt="Favorited" />
              ) : (
                <img src={Star_line} id="star" alt="Not Favorited" />
              )}
          </button>
        )}
        <div className="prod_type">
          <p>{prod.type}</p>
        </div>
        
        <div className="prod_measure">
          <p>{prod.measure}: {ulist.amount}</p>
        </div>
        
        <div className="prod_exp">
          <p>Exp: {ulist.expiration}</p>
        </div>
        {active ? (
          <button
            className="button_delete"
            onClick={() => this.props.deleteProduct(prod.id)}
          >
            <p>Delete Product</p>
            <img src={Cross} id="cross" alt="" />
          </button>
        ) : (
          <div></div>
        )}
      </div>
      </div>
    );
  }
}

// eksportering av hovedfunksjonen
export default Inventory;