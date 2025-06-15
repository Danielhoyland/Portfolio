import React, { useEffect, useState } from 'react';
import './style.css';

import AboutMe from './src/about'; 
import Header from './src/header';
import Footer from './src/footer';
import ProjectCard from './src/components/ProjectCard';
import dockerLogo from "/src/svg/docker_logo.svg";
const sampleProjects = [
  {
    title: "Weather App",
    description: "Shows weather in real time using OpenWeather API.",
    tools: [dockerLogo, dockerLogo, dockerLogo],
    images: [dockerLogo, dockerLogo],
  },
  {
    title: "Portfolio Site",
    description: "My personal portfolio built in React.",
    tools: [dockerLogo, dockerLogo, dockerLogo],
    images: [dockerLogo, dockerLogo],
  },
];
const Portfolio = () => {




  useEffect(() => {
    // Load dots.js script if needed
    const script = document.createElement('script');
    script.src = 'PortfolioWebsite/dots.js';
    script.async = true;
    document.body.appendChild(script);
    return () => {
      document.body.removeChild(script);
    };
  }, []);
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
        <div className="flex flex-wrap justify-center gap-4">
  {sampleProjects.map((project, idx) => (
    <ProjectCard key={idx} {...project} />
  ))}
</div>




        <div className="grid">
          

          <div style={{ padding: '30px', textAlign: 'center' }}>
            <h2>Project pages:</h2>
          </div>

          {/* Row 2 */}
          <div className="row">
            <a href="PortfolioWebsite/ai.html" className="grid-link">
              <div className="grid-item small">Artificial Intelligence</div>
            </a>
            <a href="PortfolioWebsite/avansert.html" className="grid-link">
              <div className="grid-item small">Advance Programming</div>
            </a>
            <a href="PortfolioWebsite/bachelor.html" className="grid-link">
              <div className="grid-item small">Bachelor Project</div>
            </a>
          </div>

          {/* Row 3 */}
          <div className="row">
            <a href="PortfolioWebsite/cloud.html" className="grid-link">
              <div className="grid-item small">Cloud Technologies</div>
            </a>
            <a href="PortfolioWebsite/database.html" className="grid-link">
              <div className="grid-item small">Database</div>
            </a>
            <a href="PortfolioWebsite/game.html" className="grid-link">
              <div className="grid-item small">Game Programming</div>
            </a>
          </div>

          {/* Row 4 */}
          <div className="row">
            <a href="PortfolioWebsite/graphic.html" className="grid-link">
              <div className="grid-item small">Graphic Programming</div>
            </a>
            <a href="PortfolioWebsite/intergrasjon.html" className="grid-link">
              <div className="grid-item small">Integration Project</div>
            </a>
            <a href="PortfolioWebsite/mobile.html" className="grid-link">
              <div className="grid-item small">Mobile Programming</div>
            </a>
          </div>
        </div>
      </main>

      {/* Footer */}
      <Footer />
    </>
  );
};

export default Portfolio;
