import React, { Component, useState, useEffect } from 'react'; //importing react for react components
import axios, {defaults} from 'axios';
import './Recipes.css'
import TopBar from './TopBar.js'
import ButtonMenu from './ButtonMenu'
import Arrow from "../icons/down-arrow-svgrepo-com.svg"
import Book from "../icons/book-icon.svg"
import {Route, Link } from "react-router-dom";

const newRecipeURL = process.env.REACT_APP_API_URL + "/new-recipe";
const updateRecipeURL = process.env.REACT_APP_API_URL + "/change-recipe";

const getIngredientFromId = (itemId, products) => {
  //searches the users products to turn the id into two different strings
  const intId = parseInt(itemId)
  if(products === null || intId === null){
    return ["Missing Item", "Missing measure type"]
  }
  const item = products.find(product => product.id === intId)
  //if the user doesn't have an item with the given id they get a missing item string instead of an error
  if(item === undefined) return ["Missing Item", "Missing measurement"]
  //return the item name and measurement
  return [item.productname, item.measure]
}

//This function is used for picking apart each step in search of items marked with #id on each side
const StepTransform = (step, quantity, products) => {
  const returnString = [];
  const items = [];
  const amount = [];
  const unit = [];
  const itemsId = [];
  //turns one string into a list of strings splitting them when it detects #id
  let splitString = step.split("#id");
  splitString.forEach((str, index) =>{
    //since every other split is just text I can simply add it to the return string
    if(index%2 === 0){
      //Just text
      returnString.push(str);
    } else{
      //item ID
      //since items have an amount part and an id part I need to split it yet again
      const tempItem = str.split(" ");
      //using getIngredientFromId function we can turn an id into the name and measurements
      const [itemName, measure] = getIngredientFromId(tempItem[0], products)
      //since the items are also going to be displayed in the step we need to add it to the returnString
      returnString.push(itemName + " ");
      //quantity will change the amount displayed in the text
      returnString.push((parseFloat(tempItem[1]) * quantity).toString());
      returnString.push(measure)

      //other functions will need access to the ids, amounts and units of measurements, so I return these values too
      items.push(itemName);
      amount.push((parseFloat(tempItem[1]) * quantity).toString());
      unit.push(measure);
      itemsId.push(parseInt(tempItem[0]))
    }
  });
  //combines the return string with a space in between
  const returnStr = returnString.join(" ")
  return [returnStr, items, amount, unit, itemsId];
}

//RecipeContainer contains all the logic of each recipe
const RecipeContainer = ({recipe, products})=>{
  //I am using a map, for simplifying the process of catching duplicates items adn adding them together later
  const totalItemsMap = new Map([[-1,[0.0, "", ""]]])
  //on the database side each recipe contains a simple string, but by adding the #newStep in the string
  //the frontend can extract a lot more data from that one string
  const tempStepDesc = recipe.recipedescription.toString().split('#newStep')
  //used for keeping track of the dropdowns at each step
  const tempSelectedItem = [products[0]]
  //each step needs to have a selected item in teh dropdown
  tempStepDesc.forEach(() => {
    tempSelectedItem.push(products[0])
  })
  //the data needed for each recipe
  //useState is a React hook and will reload the elements that us the values when setData is called
  const [data, setData] = useState({
    expanded: false,
    stepDesc: tempStepDesc,
    portion: recipe.portion,
    //If the id is -1 it means that it is expecting the new recipe
    editMode: recipe.recipeid === -1,
    recipeName: recipe.recipename,
    //used for dropdown of each step
    selectedItem: tempSelectedItem,
    ammount: [1],
  })
  //each time you add a new step you need to populate the dropdowns with some information

  const loadSteps = () => {
    const tempSelect = []
    const tempAmmount = []
    data.stepDesc.forEach((_, index) =>{
      tempSelect[index] = {
        id: -1,
        productname: 'selectItem',
        measure: 'noDef'
      }
      tempAmmount[index] = 1
    })
    //setData is reloading the page, so it needs to be called sparingly
    setData({
      ...data, selectedItem: tempSelect, ammount: tempAmmount
    })
  }

  //expanded controls what html elements are displayed
  const toggleExpand = () => {
    setData({
      ...data, expanded: !data.expanded
    })
  }

  //newStep adds another step to each recipe
  const newStep = () => {
    const temp = data.stepDesc
    temp.push("")
    setData({
      ...data, stepDesc: temp
    })
    loadSteps()
  }

  //removeStep uses the splice function to remove the string at location that is stored withing the event (e)
  const removeStep = (e) => {
    const temp = data.stepDesc
    temp.splice(parseInt(e.target.name), 1)
    setData({
      ...data, stepDesc: temp
    })
  }

  //the buttons used for adding / removing steps
  const recipeButtons = (index) => {
    //have to make sure that the suer doesn't remove the first step or else the user could have the option of uploading
    //a recipe without description
    return(
        <div className="recipe-step-container-desc-edit-buttons">
          <button className="recipe-step-container-edit-button-newStep"
                  onClick={newStep}>New Step</button>
          {index !== 0 ? (<button className="recipe-step-container-edit-button-removeStep"
                                  name={index}
                                  onClick={removeStep}>Remove Step</button>) :
              (<button className="recipe-step-container-edit-button-removeStep-greyed"
                       name={index}
                       onClick={console.log("Can't remove first step")}>Remove Step</button>)}
          {dropDown(index)}
        </div>
    )
  }

  //Drop down options is a list of all items the user has added combined into one dropdown
  const dropdownOptions = []
  products.forEach((item) => {
    //the data fo each item is stored withing each option for later retrieval later
    dropdownOptions.push(<option value={item.id} name={item.productname} type={item.measure}>{item.productname + ":" + item.measure}</option>)
  })
  const dropDown = (index) => {
    if(data.selectedItem.length === 0) loadSteps()
    const addItemId = (event) =>{
      event.preventDefault()
      //makes sure that the item in the dropdown menu is a valid item
      if(data.ammount[index] > 0 && data.selectedItem[index].id !== -1){
        //modifies the string at the given recipe by adding the item notation as seen bellow
        const tempDesc = data.stepDesc
        //items needs to be encased between two #id with the id first then the amount
        //the recipe also takes into consideration what the portion is at the time of adding the item
        tempDesc[event.target.name] += "#id" + data.selectedItem[index].id + " " + data.ammount[index] / recipe.portion + "#id"
        setData({
          ...data, stepDesc: tempDesc
        })
      }
    }
    //whenever the dropdown menu is modified
    const dropdownChange = (event) => {
      const temp = data.selectedItem
      //event stores the data within each option to reconstruct the product
      temp[index] = {
        id: event.target.value,
        productname: event.target.name,
        measure: event.target.type
      }
      setData({
        ...data, selectedItem: temp
      })
    }
    //Used when changing amounts
    const amountChange = (event) => {
      const input = event.target.value
      //makes sure that the new value is numerical
      if(/^-?\d*\.?\d*$/.test(input)){
        const temp = data.ammount
        temp[index] = input
        setData({
          ...data, ammount: temp
        })
      }
    }

    //placing the dropdown within one form allows the user to maneuver using tab and submit by either the button or enter
    return (
    <div className="recipe-step-container-edit-add-item">
      <form  name={index} className="inherit" onSubmit={addItemId}>
        <label className="recipe-step-container-edit-button-dropdown">
          <select onChange={dropdownChange} >
            {dropdownOptions}
          </select>
        </label>
        <input className="recipe-step-container-edit-button-amount-input" type="text" value={data.ammount[index]} onChange={amountChange} placeholder="0.0"/>
        <button className="recipe-step-container-edit-button-submit">Add Item</button>
      </form>
    </div>)
  }

  //collapse is a helping function for turning the data struct into a json that can be parsed back to the database
  const collapse = () => {
    const productIDs = []
    const productsAmount = []
    totalItemsMap.forEach((value, key) => {
      if(key !== -1){
        productIDs.push(key)
        productsAmount.push(parseFloat(value[0]))
      }
    })
    //stops the suer form calling the recipe for New Recipe
    if(data.recipeName === "New Recipe") return null
    //have to add back the #newStep when combining the steps, since the database only takes one string as description
    const returnString = data.stepDesc.join('#newStep')
    //the database also need the session token in order to know what user saved the recipe
    return {
      user : {
        sessiontoken: sessionStorage.getItem('sessionToken')},
      recipe: {
        recipename: data.recipeName,
        recipedescription: returnString,
        Portion: data.portion,
        recipeId: recipe.recipeid},
      trequiredproducts: productIDs,
      tamount: productsAmount
    }
  }

  const handleDescriptionChange = (e) => {
    const value = e.target.value;
    const tempDesc = data.stepDesc
    //the name of the input field is the same as the index of each of the steps
    tempDesc[e.target.name] = value
    setData({
      ...data, stepDesc: tempDesc
    })
  }

  //whenever the user changes teh name of the recipe
  const handleNameChange = (e) => {
    const value = e.target.value;
    setData({
      ...data, recipeName: value
    })
  }

  //Each recipe ahs an edit mode and a display mode, this function swaps between when called
  const toggleEdit = () => {
    loadSteps()
    setData({
      ...data, editMode: !data.editMode
    })
  }

  const deleteRecipe = () => {
    axios.delete(newRecipeURL, {
      data: {
        user: {
          sessiontoken: sessionStorage.getItem("sessionToken")
        },
        recipe: {
          recipeid: recipe.recipeid
        }
    }})
        .then(res => window.location.href = '/myfridge/recipes')//reloads the page after delete
        .catch(error => console.log(error.toString()))
  }

  //using the collapse function save changes pushes the changes done to the recipe back to the database
  const saveChanges = () => {
    console.log("Json of data to be saved: " +JSON.stringify(collapse(), null, 2))
    if(recipe.recipeid === -1){ //If the recipe is the new recipe
      axios.post(newRecipeURL, collapse()).then(res => window.location.href = '/myfridge/recipes')
    } else { //if the recipe already exists
      axios.post(updateRecipeURL, collapse()).then(res => window.location.href = '/myfridge/recipes')
    }

  }

  const steps = [];
  //breaks down each step
  data.stepDesc.forEach((step, index) =>{
    const [StepText, Items, Amount, Unit, ItemsId] = StepTransform(step, data.portion, products);

    //for the summed up items at the top of each recipe
    for (let i = 0; i < Items.length; i++){
      const key = ItemsId[i]
      const ammountValue = Amount[i]
      const unitOfMessurement = Unit[i]
      const itemName = Items[i]
      //if the item is already present in the map, we simply add the new value
      if(totalItemsMap.has(key)){
        const currentValue = totalItemsMap.get(key)
        const newValue = parseFloat(currentValue[0]) + parseFloat(ammountValue)
        totalItemsMap.set(key, [newValue, itemName, unitOfMessurement])
      } else {
        totalItemsMap.set(key, [ammountValue, itemName, unitOfMessurement])
      }
    }
    //used fore styling of the recipe
    steps.push(
        <div className="recipe-spacer"/>
    )
    //the recipe has different views based on if it is in edit mode or not
    if(data.editMode){
      //in edit mode the buttons are going to be accessible, and the area where the description is stored is instead
      //an input field
      steps.push(
          <div className="recipe-step-container-desc-edit"><h3>{index + 1}:</h3>
            <textarea
                className="recipe-step-container-desc-edit-input"
                id="stepDesc"
                name={index.toString()}
                value={data.stepDesc[index]}
                onChange={handleDescriptionChange}
                placeholder="Enter description" required/>
          </div>
      )
      steps.push(
          recipeButtons(index)
      )
    }else{
      steps.push(
          <div className="recipe-step-container-desc"><h3>{index + 1}:</h3>{StepText}</div>
      );
    }
    //displays the ingredients of each step
    steps.push(
        <div className="recipe-step-container-ingredients">
          {IngredientsToHTML(Items, Amount, Unit)}
        </div>)
  });

  const portionChange = (event) => {
    const input = event.target.value
    //makes sure that the portion is numerical
    if(/^-?\d*\.?\d*$/.test(input)){
      const inputInt = parseInt(input)
      setData({
        //makes sure that the new portion is above 0
        ...data, portion: (inputInt > 0 ? inputInt : '')
      })
    }
  }

  const totalIngredientsDisplay = () => {
    //displays total ingredients
    const display = []
    let index = 0
    totalItemsMap.forEach(function(value, key){
      if(key !== -1){
        if(index !== 0 && index % 2 === 0){
          display.push(<div className="recipe-ingredient-1span"/>)
        }
        display.push(<div className="recipe-ingredient-2span">{value[1]}</div>)
        display.push(<div className="recipe-ingredient-1span">{value[0]}</div>)
        display.push(<div className="recipe-ingredient-2span">{value[2]}</div>)
        index ++
      }
    })

    return display
  }
  return(
      <div className={`recipe-container ${data.expanded ? 'recipe-expanded' : ''}`}>
        <div className="recipe-header" >
          {(data.editMode && data.expanded) ?
              (<textarea
                  className= "recipe-header-title-edit"
                  value={data.recipeName}
                  onChange={handleNameChange}
                  placeholder="Recipe name" required/>) :
              (<div className="recipe-header-title"> {data.recipeName}</div>)}
          <img src={Arrow} className={`recipe-header-icon ${data.expanded ? 'header-icon-flipped' : ''}`} alt={"Arrow"} onClick={toggleExpand}/>
        </div>
        {data.expanded ? (
            <div className="recipe-content-container">
              <button onClick={toggleEdit}>Edit mode toggle</button>
              {(data.editMode && recipe.recipeid !== -1) ? (<button onClick={deleteRecipe}>Delete Recipe</button>) : null}
              {data.editMode ? (<button onClick={saveChanges}> Save Recipe</button>) : null}
              <text className="extra-padding">Portions:</text>
              <input type="text" onChange={portionChange} value={data.portion}/>
              <div className="recipe-ingredients-container">
                {totalIngredientsDisplay()}
              </div>
              {steps}
            </div>
        ) : ('')}
      </div>
  );
};
const IngredientsToHTML = (Item, Amount, Unit)=>{
  const returnIngredients = [];
  Item.forEach((item, index) =>{
    returnIngredients.push(
        <h5>{item + ": " + Amount[index] + Unit[index]}</h5>
    )
  });
  return returnIngredients;
}

// login component
class Recipes extends Component {
  constructor(props){
    super(props)
    //Data that Recipes will need
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
        type:""
      },
      recipes: [],
      recipe: {
        name:"",
        description:"",
        portion: 0,
        recipeid: 0
      },
      addrecipe: {
        reqProducts:[],
        ammount:[]
      }
    }
  }


  componentDidMount() {
    //instead of requesting data with the email or user id we instead went for a session token approach
    //This makes the session extra secure and ensures that two people can't be logged in at the same time
    const sessionToken = sessionStorage.getItem("sessionToken")
    //Used for fetching data from the database and populating the tables above
    if (sessionToken) {
      this.props.getData()
    } else {
      //Invalid session token boots the user back
      window.location.href = 'http://10.212.170.10:3000';
    }
  };


  render () {
    //system put in place in case of a race condition
    const ReloadComponent  = () => {
      //useEffect is needed for setting a timer
      useEffect(() => {
        const timerId = setTimeout(() => {
          //reloads the location after 2000milli seconds
          window.location.reload()
        }, 2000);
        return () => clearTimeout(timerId)
      }, []);

      return <div>loading</div>
    }
    //is called if recipes isn't fetched before the rest of the website loads
    if(this.props.data.recipes === null) {
      ReloadComponent()
    }
    //Used to store the recipe components
    const recipeList = [];
    //I treat every Recipe like its own tiny website
    //This allows user to make changes to one recipe without impacting other recipes

    //this first one is the new recipe and since the user isn't allowed to remove it, I chose to hard code it inn
    //recipeId is set to -1, this is used later for calling newRecipe instead of updateRecipe
    recipeList.push(<RecipeContainer recipe={{recipename: "New Recipe", recipeid: -1, portion: 1, recipedescription: ""}}
                                        products={this.props.data.products}/>)
    this.props.data.recipes.forEach((recipe, index) =>{
     //populates the recipe page with recipes that the user has
      recipeList.push(<RecipeContainer recipe={recipe} products={this.props.data.products}/>)
    })

    return (
      <main id = "page_recipes">
        <div class = "sidebar" id = "sidebar_right"/>
        <div class = "sidebar" id = "sidebar_left"/>
        <TopBar username = {this.props.data.user.username} text = "Recipes"/>
        <div className="recipe-area">
          {recipeList}
        </div>
        <ButtonMenu/>
      </main>
    );
  }
}

export default Recipes;