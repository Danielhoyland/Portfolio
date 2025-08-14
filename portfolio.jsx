import React, { useEffect, useState } from "react";
import "./style.css";

import AboutMe from "./src/about";
import Header from "./src/header";
import Footer from "./src/footer";
import AI from "./src/pages/ai";
import FlexGrid from './src/components/Flexcard';

const Portfolio = () => {
  const handleComponentLoad = (target) => {
    console.log(`Navigate to component: ${target}`);
  };
  return (
    <>
      {/* Header */}
      <Header />

      {/* Main Portfolio Grid */}
      <main className="portfolio-content">
        <AboutMe />
        <div style={{ display: "flex", gap: "16px" }}>
          <FlexGrid itemsPerRow={4} gap="16px" style={{ width: "100%" }}>
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
        </div>
      </main>

      {/* Footer */}
      <Footer />
    </>
  );
};

export default Portfolio;
