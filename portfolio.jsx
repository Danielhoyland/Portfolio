import React, { useEffect, useState } from "react";
import "./style.css";

import AboutMe from "./src/about";
import Header from "./src/header";
import Footer from "./src/footer";
import Advance from "./src/pages/advance";
import AI from "./src/pages/ai";
import Algo from "./src/pages/algo";
import Bach from "./src/pages/bachelor";
import Cloud from "./src/pages/cloud";
import DB from "./src/pages/database";
import Game from "./src/pages/game";
import Graphic from "./src/pages/graphic";
import Inte from "./src/pages/integration";
import Mobile from "./src/pages/mobile";
import FlexGrid from './src/components/Flexcard';


const Portfolio = () => {
  return (
    <>
      {/* Header */}
      <Header />

      {/* Main Portfolio Grid */}
      <main className="portfolio-content">
        <AboutMe />
          <FlexGrid gap="16px">
            <Advance />
            <AI />
            {/*<Algo />*/}
            <Bach />
            <Cloud /> 
            <DB /> 
            <Game /> 
            <Graphic /> 
            <Inte /> 
            <Mobile /> 
          </FlexGrid>
      </main>

      {/* Footer */}
      <Footer />
    </>
  );
};

export default Portfolio;
