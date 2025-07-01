import React, { useEffect, useState } from "react";
import "./style.css";


import AboutMe from "./src/about";
import Header from "./src/header";
import Footer from "./src/footer";
import AI  from "./src/ai";



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
          
          <AI />
          <AI />
          <AI />
          <AI />
        </div>
      </main>

      {/* Footer */}
      <Footer />
    </>
  );
};

export default Portfolio;
