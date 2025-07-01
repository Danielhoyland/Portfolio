import React from 'react';
import '../style.css'; 
import logo from './images/Logo-Ntnu.png';


const Header = () => {
  return (
    <header className="site-header">
        <br></br>
      <h1>
        <a className="index-link" href="/index.html">Daniel Høyland's Portfolio</a>
      </h1>
      <div className="header-inline">
  <img
    src={logo}
    className="logo"
    alt="Daniel Høyland"
  />
  <h2>Master In Informatics student</h2>
  <img
    src={logo}
    className="logo"
    alt="Daniel Høyland"
  />
</div>

      <br></br>
    </header>
  );
};

export default Header;

