import React, { Component } from 'react';
import './Settings.css';
import ButtonMenu from './ButtonMenu';
import TopBar from './TopBar';

// hovedfunksjonen
class Settings extends Component {

    // kjører når komponenten åpnes
    // redirekterer til login siden om bruker er ikke logget inn
    componentDidMount() {
      const sessionToken = sessionStorage.getItem('sessionToken');
      if (sessionToken) {
        this.props.getData();
      } else {
        window.location.href = '/login';
      }
    }

    // kaller funsjonen for å slette brukerdata
    deleteUserButton = () => {
        this.props.deleteUser();
    };

    // hoved html
    render(){
      return (
        <main id = "page_home">
          <div class = "sidebar" id = "sidebar_right"/>
          <div class = "sidebar" id = "sidebar_left"/>
          <ButtonMenu/>
          <button id = "delete_user" onClick={() => this.deleteUserButton()}>
            <p>DELETE ALL USERDATA</p>
          </button>
          <TopBar username = {this.props.data.user.username} text = "Home"/>
        </main>
      );
    }
  }

// eksportering av hovedfunksjon
export default Settings;