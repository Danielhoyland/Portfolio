import React, { Component } from 'react';
import './Home.css';
// importering av brukte komponenter
import ButtonMenu from './ButtonMenu';
import TopBar from './TopBar';
import { Product } from './Inventory';

// Hovedfunksjonen for siden
class Home extends Component {

  // Kjører når komponenten blir åpnet
  // Sjekker om brukeren er innlogget, hvis ikke fører tilbake til logg-inn siden
  componentDidMount() {
    const sessionToken = sessionStorage.getItem('sessionToken');
    if (sessionToken) {
      this.props.getData();
    } else {
      window.location.href = '/login';
    }
  }

  // oppdatter data
  UpdateData = async () => {
    await this.props.getData();
  };

  // hoved html
  render(){
    return (
      <main id = "page_home">
        {/* Sidelinjene på desktop oppsettet */}
        <div class = "sidebar" id = "sidebar_right"/>
        <div class = "sidebar" id = "sidebar_left"/>
        <ButtonMenu/>
        <TopBar username = {this.props.data.user.username} text = "Home"/>
        <div class="subtitle" id="subtitle_expiring">
          <h3>Expiring</h3>
        </div>
        {/* Komponenten for produkter som går ut på dato */}
        <ExpiringItems data = {this.props.data}/>
        <div class="subtitle" id="subtitle_favorites">
          <h3>Favorites</h3>
        </div>
        {/* Komponenten for favorittprodukter */}
        <Favorites data={this.props.data} deleteProduct={this.props.deleteProduct}/>
      </main>
    );
  }
}

// Komponenten for produkter som går ut på dato
class ExpiringItems extends Component{
  render(){
    const { data } = this.props;
    // grense på hvor mange dager frem i tid skal programmet vise utgående produkter
    const maxExpDays = 5; 
    const today = new Date();     // henter dagens dato
    today.setHours(0, 0, 0, 0);   // setter på null timer
    // lager dato for "maxExpDays" langt fram i tid
    const nextFiveDays = new Date();  
    nextFiveDays.setDate(today.getDate() + maxExpDays);
    
    // hvis det er tomt for produkter, ikke vis noe
    if (data.products == null) {
      return <div></div>;
    } else {
      // filtrerer ut dager som ikke skal vises
      // (som ikke er innom de neste 5 dagene)
        const expProducts = data.userlists.filter((ulist, i) => {
        const expDate = new Date(ulist.expiration);
        return expDate >= today && expDate <= nextFiveDays;
      });
      console.log("Products: " + data.products[0]);
      return (
        <div id="expiringitems" className="hscroll">
          {/* Mapper gjennom dagene som skal bli vist (de innom de neste 5 dager)*/}
          {expProducts.map((ulist, i) => {
            console.log(ulist);
            // finner produktet som tilhører userlists id'en 
            const prod = data.products.find(product => product.id === ulist.productid);
            const expDate = new Date(ulist.expiration);
            // kalkulerer millisekunder mellom dagene, så konverterer fra millisekunder til dager
            const remDays = Math.abs(Math.floor((expDate - today) / (1000*60*60*24)));
            if (!prod) {
              console.error(`Product not found for id: ${ulist.productid}`);
              return null; 
            }
            return (
              <div key={i} className="ex_item">
                <div className="ex_item_layout">
                  <p className="ex_name">{prod.productname}</p>
                  <p className="ex_measure">{prod.measure + ": " + ulist.amount}</p>
                  <p className="ex_days">{(remDays === 0 ? "TODAY!" : remDays+" Day(s)!")}</p>
                  <p className="ex_exp">{ulist.expiration}</p>
                </div>
              </div>
            )
          }
          )}
        </div>
      );
    }
  }
}

// Favoritt produkter
class Favorites extends Component {
  render() {
    const { data } = this.props;
    if (data.products == null){
      return(<div></div>);
     }else{
      return (
        <div id="favoriteitems">
          {data.products.map((prod, i) =>
            data.userlists[i].favorite ? (
              // sender med relevant data til produkt komponenten
              <Product key={i} prod={prod} ulist={data.userlists[i]} active={false} deleteProduct={this.props.deleteProduct}/>
            ) : (
              null
            )
          )}
        </div>
      );
    }
  }
}


// eksportering av hovedkomponenten
export default Home;