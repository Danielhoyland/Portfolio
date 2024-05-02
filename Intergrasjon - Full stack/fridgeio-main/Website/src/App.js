import './App.css';
import axios from 'axios';
import React, { Component } from 'react';
import { BrowserRouter as Router, Routes, Route } from 'react-router-dom';
import Home from './components/Home'; // importer homepage
import Login from './components/Login'; // importer loginpage
import Inventory from './components/Inventory'; // importer inventory page
import Recipes from './components/Recipes'; // importer recipes page
import Settings from './components/Settings'; //importer settings page


// url'er for API calls
const getDataurl = process.env.REACT_APP_API_URL + '/getData';
const addproducturl = process.env.REACT_APP_API_URL + '/new-item';
const addrecipeurl = process.env.REACT_APP_API_URL + '/new-recipe';
const deleteUserurl = process.env.REACT_APP_API_URL + '/new-user';

// hovedkomponenten
class App extends Component {

  constructor(props){
    super(props)

    // data som blir lagret lokalt når man er inne på nettsiden
    // dataen som blir sendt gjennom til forskjellige komponenter
    this.state ={
      user: {
        username:"",
        email: ""
      },
      products: [],
      userlists: [],
      product: {
        productname:"",
        measure:"",
        type:"",
        amount:0,
        expiration:"",
        imageid:0,
        favorite:false
      },
      recipes: [],
      recipe: {
        name:"",
        description:"",
        portion: 0,
      },
      addrecipe: {
        reqProducts:[],
        ammount:[]
      }
    }

  }

  // funksjonen for å hent inn oppdattert database data
  getData = async () => {
    axios.post(getDataurl, {
      sessiontoken: sessionStorage.getItem("sessionToken")
    })
    .then((res)=>{
      console.log("WENT THROUGH, GOT DATA:", res.data)
      if (res.data){
        this.setState({
          user: res.data.user,
          products: res.data.products !== null ? res.data.products : [],
          userlists: res.data.userlists !== null ? res.data.userlists : [],
          recipes: res.data.recipes !== null ? res.data.recipes : [],
            recipeInfos: res.data.recipeInfos
        })    
      }else{
        this.setState({
          user: null,
          products: [],
          userlists: [],
          recipes: []
        })
      }
    })
    .catch((error) => {
      console.log(error);
    })
  }

  // funksjonen for å adde ny produkt til databasen
  addProduct = (e) => {
    e.preventDefault();
    console.log("data to be sent: ", this.state.product)
    axios.post(addproducturl, {
      user: {
        sessiontoken: sessionStorage.getItem("sessionToken")
      },
      product: {
        productname: this.state.product.productname,
        measure: this.state.product.measure,
        type: this.state.product.type
      },
      userlist: {
        expiration: this.state.product.expiration,
        amount: this.state.product.amount,
        imageid: this.state.product.id
      }
    })
    .then((res)=>{
      this.getData();
      this.setState({
        product: {
          productname:"",
          measure:"",
          type:""
        },
        recipe: {
          name:"",
          description:"",
          portion: 0,
        },
        addrecipe: {
          reqProducts:[],
          ammount:[]
        }
      })
    })
    .catch((error) => {
      console.log(error);
    });
  }

  // funksjon for å legg til ny oppskrift til databasen
  addRecipe = (e) => {
    e.preventDefault();
    console.log("data to be sent: ", this.state.recipe)
    axios.post(addrecipeurl, {
      user: {
        sessiontoken: sessionStorage.getItem("sessionToken")
      },
      TRecipe: this.state.recipe,
      TRequiredProducts: this.state.addrecipe.reqProducts,
      TAmount: this.state.addrecipe.ammount
    })
        .then((res) => {
          this.getData()
          this.setState({
            product: {
              productname:"",
              measure:"",
              type:""
            },
            recipe: {
              name:"",
              description:"",
              portion: 0,
            },
            addrecipe: {
              reqProducts:[],
              ammount:[]
            }
          })
        }).catch((error) => {
      console.log(error)
    })
  }

  // funksjonen for å slette brukerdata fra databasen, så logg ut brukeren
  deleteUser = (e) => {
    const sessionToken = sessionStorage.getItem("sessionToken");
    axios.delete(deleteUserurl, {
      data: {
        sessiontoken: sessionToken
      }
    })
    .then((res)=>{
      console.log("DELETION SUCCEEDED");
      window.location.href = '/';
    })
    .catch((error) => {
      console.log("ERROR IN DELETION", error);
    })
  }

  // funksjonen for å oppdattere formen når noe endres
  onChange = (event) => {
    const { name, value } = event.target;
    if (name == "amount"){
      this.setState((prevState) => ({
        product: {
          ...prevState.product,
          [name]: Number(value)
        }
      }));
    }else{
      this.setState((prevState) => ({
        product: {
          ...prevState.product,
          [name]: value
        }
      }));
    }
  };

  // hoved html'en
  // bestemmer hvilke komponneter som kjører på spesifike URL endinger
  render () {
    return (
      <Router>
        <Routes>
          {/* Logg inn siden */}
          <Route exact path="/" element={<Login />} /> 
          <Route path="/myfridge" element={
            <Home 
              data={this.state} 
              getData={this.getData} 
            />
          }/>
          {/* Inventar siden */}
          <Route path="/myfridge/inventory" element={
            <Inventory
              data={this.state} 
              getData={this.getData} 
              onChange={this.onChange}
              addProduct={this.addProduct}
              addproducturl={addproducturl} 
            />
          }/>
          {/* Oppskrift siden */}
          <Route path ="/myfridge/recipes" element={
            <Recipes
              data={this.state} 
              getData={this.getData}
              getRecipes={this.getRecipes}
              addRecipe={this.addRecipe}
            />
          }/>
          {/* Innstillinger siden */}
          <Route path ="/myfridge/settings" element={
            <Settings
              data={this.state} 
              getData={this.getData}
              deleteUser = {this.deleteUser}
            />
          }/>
        </Routes>
      </Router>
    );
  }
}

// eksporterer hovedkomponenten
export default App;