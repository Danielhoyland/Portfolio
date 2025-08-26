import React from "react";
import "./css/header.css";

const Header = () => {
  return (
    <header className="site-header">
      <br></br>
      <h1>
        <a className="index-link" href="/Portfolio/">
          Daniel Høyland's Portfolio
        </a>
      </h1>
      <h2>Master In Informatics student</h2>
      <br></br>
    </header>
  );
};

export default Header;
