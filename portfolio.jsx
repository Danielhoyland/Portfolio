import React, { useState,  useMemo } from "react";
import "./style.css";

import { IoFilter, IoMenu } from "react-icons/io5";
import { motion, AnimatePresence } from "framer-motion";


import AboutMe from "./src/about";
import Header from "./src/header";
import Footer from "./src/footer";
import Advance from "./src/pages/advance";
import AI from "./src/pages/ai";
import Applied from "./src/pages/applied"
import Algo from "./src/pages/algo";
import Bach from "./src/pages/bachelor";
import Cloud from "./src/pages/cloud";
import DB from "./src/pages/database";
import Game from "./src/pages/game";
import Graphic from "./src/pages/graphic";
import Inte from "./src/pages/integration";
import Mobile from "./src/pages/mobile";
import Model from "./src/pages/model";
import Gym from "./src/pages/gym";
import Bryllup from "./src/pages/bryllup";
import ScriptBlackboard from "./src/pages/scriptBlackboard";
import NotificationDiscord from "./src/pages/notificationDiscord";
import InformationRetrieval from "./src/pages/informationRetrieval";
import Personvern from "./src/pages/personvern";
import CollectingCardsMaker from "./src/pages/collectingCardsMaker";
import EkspertInTeams from "./src/pages/ekspertInTeams";
import AdvancedSoftwareEngineering from "./src/pages/advancedSoftwareEngineering";
import AppliedData from "./src/pages/appliedData";
import MachineLearning from "./src/pages/machineLearning";
import FlexGrid from './src/components/Flexcard';


const Portfolio = () => {

  
const icons = [
  { component: IoFilter, rotate: 0 },
  { component: IoMenu, rotate: 180 },
  { component: IoFilter, rotate: 180 }, // upside-down
];
  const [index, setIndex] = useState(0);



  const [toggleMode, setToggleMode] = useState(0);

  const projectArray = [
    [<Advance />, "Bachelor", "Advance Programming", ["Haskell", "Rust", "SDL2", "Go"]],
    [<AI />, "Bachelor", "AI", ["Python", "TensorFlow"]],
    [<Applied />, "Master", "Applied Data Science", ["Python"]],
    //[<Algo />, "Bachelor", "Algorithms", ["C++", "Python"]],
    [<Bach />, "Bachelor", "Bachelor", []],
    [<Cloud />, "Bachelor", "Cloud Programming", ["AWS", "Docker"]],
    [<DB />, "Bachelor", "Database", ["SQL", "MongoDB"]],
    [<Game />, "Bachelor", "GameProg", ["C#", "Unity"]],
    [<Graphic />, "Bachelor", "Graphics", ["Blender", "OpenGL"]],
    [<Inte />, "Bachelor", "Integration Project", ["Node.js", "React"]],
    [<Mobile />, "Bachelor", "Mobile Programming", ["Flutter", "Kotlin"]],
    [<Model />, "Master", "Model-Driven Software Engineering", ["Python"]],
    [<Gym />, "Personal", "Gym", []],
    [<Bryllup />, "Personal", "Bryllup", []],
    [<ScriptBlackboard />, "Personal", "Script for Blackboard", []],
    [<NotificationDiscord />, "Personal", "Notification Integration with Discord", []],
    [<InformationRetrieval />, "Master", "Information Retrieval", []],
    [<Personvern />, "Personal", "Personvern", []],
    [<CollectingCardsMaker />, "Personal", "Collecting Cards Maker", []],
    [<EkspertInTeams />, "Personal", "Ekspert in Teams", []],
    [<AdvancedSoftwareEngineering />, "Master", "Advanced Software Engineering", []],
    [<AppliedData />, "Master", "Applied Data", []],
    [<MachineLearning />, "Master", "Machine Learning", []],
  ];

  // Compute grouped projects based on toggleMode
  const groupedProjects = useMemo(() => {
    if (toggleMode === 0) {
      // No grouping, just sort alphabetically by name
      return projectArray
        .slice()
        .sort((a, b) => a[2].localeCompare(b[2]));
    }

    const groups = {};
    projectArray.forEach(([Component, category, name, techs]) => {
      if (toggleMode === 1) {
        // Group by category
        if (!groups[category]) groups[category] = [];
        groups[category].push([Component, name]);
      } else if (toggleMode === 2) {
        // Group by technology
        if (techs && techs.length > 0) {
          techs.forEach((tech) => {
            if (!groups[tech]) groups[tech] = [];
            groups[tech].push([Component, name]);
          });
        }
      }
    });

    // Sort project names within each group alphabetically
    Object.keys(groups).forEach((key) => {
      groups[key].sort((a, b) => a[1].localeCompare(b[1]));
    });

    return groups;
  }, [toggleMode]);

  // Get button label for next mode
  const getButtonText = () => {
    if (toggleMode === 0) return "Group by Category";
    if (toggleMode === 1) return "Group by Technology";
    return "Show All Projects";
  };

 const handleToggle = () => {
    setIndex((prev) => (prev + 1) %  icons.length);
    setToggleMode((prev) => (prev + 1) % 3);
  };
    const CurrentIcon = icons[index].component;
  return (
    <>
      <Header />
      <main className="portfolio-content">
        <AboutMe />

       <button
      onClick={handleToggle}
      style={{
        background: "transparent",
        border: "none",
        cursor: "pointer",
        padding: "8px",
        display: "flex",
        alignItems: "center",
        justifyContent: "center",
      }}
    >
      <motion.div
        key={index}
        animate={{ rotate: icons[index].rotate, scale: 1.2 }}
        initial={{ scale: 0.8 }}
        transition={{ type: "spring", stiffness: 300, damping: 20 }}
        style={{ display: "flex", alignItems: "center", justifyContent: "center" }}
      >
        <AnimatePresence mode="wait">
          <motion.div
            key={index + "-icon"}
            initial={{ opacity: 0, scale: 0.8 }}
            animate={{ opacity: 1, scale: 1 }}
            exit={{ opacity: 0, scale: 0.8 }}
            transition={{ duration: 0.3, ease: "easeInOut" }}
            style={{ position: "absolute" }}
          >
            <CurrentIcon size={28} />
          </motion.div>
        </AnimatePresence>
      </motion.div>
    </button>

        {toggleMode === 0 ? (
          <FlexGrid gap="16px">
            {groupedProjects.map(([Component, _, name], idx) => (
              <div key={idx}> 
                {Component}
              </div>
            ))}
          </FlexGrid>
        ) : (
          // Grouped view (category or technology)
          Object.keys(groupedProjects)
            .sort() // sort group headers alphabetically
            .map((groupKey) => (
              <div key={groupKey} style={{ marginBottom: "24px" }}>
                <h2>{groupKey}</h2>
                <FlexGrid gap="16px">
                  {groupedProjects[groupKey].map(([Component, name], idx) => (
                    <div key={idx}>
                      {Component}
                    </div>
                  ))}
                </FlexGrid>
              </div>
            ))
        )}
      </main>
      <Footer />
    </>
  );
};

export default Portfolio;
