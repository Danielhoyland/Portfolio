import React, { useEffect, useState } from "react";
import "./style.css";

import AboutMe from "./src/about";
import Header from "./src/header";
import Footer from "./src/footer";
import AI from "./src/pages/ai";
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
            <AI />
            <AI />
            <AI />
            <AI />
            <AI /> 
            <AI /> 
            <AI /> 
            <AI /> 
            <AI /> 
            <AI /> 
            <AI /> 
            <AI /> 
          </FlexGrid>
      </main>

      {/* Footer */}
      <Footer />
    </>
  );
};

export default Portfolio;
