import React, { useEffect, useState } from "react";
import "./style.css";

import AboutMe from "./src/about";
import Header from "./src/header";
import Footer from "./src/footer";
import AI from "./src/pages/ai";
import FlexGrid from './src/components/Flexcard';


const Portfolio = () => {

 const [itemsPerRow, setItemsPerRow] = useState(4);

  useEffect(() => {
    const handleRows = () => {
      if (window.innerWidth < 400) {
        setItemsPerRow(1);
      } else if (window.innerWidth < 640) {
        setItemsPerRow(2);
      } else {
        setItemsPerRow(4);
      }
    };

    handleRows(); // run once on mount
    window.addEventListener("resize", handleRows);

    return () => window.removeEventListener("resize", handleRows);
  }, []);

  return (
    <>
      {/* Header */}
      <Header />

      {/* Main Portfolio Grid */}
      <main className="portfolio-content">
        <AboutMe />
        <div style={{ display: "flex" }}>
          <FlexGrid itemsPerRow={itemsPerRow} style={{ width: "100%" }}>
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
