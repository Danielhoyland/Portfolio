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
import MachineLearning from "./src/pages/machineLearning";
import FlexGrid from './src/components/Flexcard';


const Portfolio = () => {

  
const icons = [
  { component: IoFilter, rotate: 0 },
  { component: IoMenu, rotate: 180 },
  { component: IoFilter, rotate: 180 }, // upside-down
];
  const [index, setIndex] = useState(0);



  const [sortMode, setSortMode] = useState("category");

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
    [<EkspertInTeams />, "Master", "Ekspert in Teams", []],
    [<AdvancedSoftwareEngineering />, "Master", "Advanced Software Engineering", []],
    [<MachineLearning />, "Master", "Machine Learning", []],
  ];

  const groupedProjects = useMemo(() => {
  // No sorting / grouping
  if (sortMode === "none") {
    return projectArray;
  }

  const groups = {};

  if (sortMode === "category") {
    projectArray.forEach(([Component, category, name]) => {
      if (!groups[category]) groups[category] = [];
      groups[category].push([Component, name]);
    });

    Object.keys(groups).forEach((key) =>
      groups[key].sort((a, b) => a[1].localeCompare(b[1]))
    );

    return groups;
  }

  if (sortMode === "technology") {
    projectArray.forEach(([Component, category, name, techs]) => {
      if (!techs.length) return;

      techs.forEach((tech) => {
        if (!groups[tech]) groups[tech] = [];
        groups[tech].push([Component, name]);
      });
    });

    Object.keys(groups).forEach((key) =>
      groups[key].sort((a, b) => a[1].localeCompare(b[1]))
    );

    return groups;
  }

  return {};
}, [sortMode]);

const categoryOrder = ["Personal", "Master", "Bachelor"];
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
  const sortControl = (
    <div className="sort-control">
      <label htmlFor="sort-select">Sort by:</label>
      <select
        id="sort-select"
        value={sortMode}
        onChange={(e) => setSortMode(e.target.value)}
      >
        <option value="category">Project Type</option>
        <option value="technology">Programming Language / Technology</option>
        <option value="none">None</option>
      </select>
    </div>
  );

  return (
    <>
      <Header />
      <main className="portfolio-content">
        <AboutMe />

        {sortMode === "none" ? (
  <>
    <div className="group-heading-row group-heading-row--controls-only">
      {sortControl}
    </div>
    <FlexGrid gap="16px">
      {groupedProjects.map(([Component], idx) => (
        <div key={idx}>{Component}</div>
      ))}
    </FlexGrid>
  </>
) : (
  Object.keys(groupedProjects)
    .sort((a, b) => {
      if (sortMode === "category") {
        const order = ["Personal", "Master", "Bachelor"];
        return order.indexOf(a) - order.indexOf(b);
      }
      return a.localeCompare(b);
    })
    .map((group, groupIndex) => (
      <div key={group} style={{ marginBottom: "24px" }}>
        <div className="group-heading-row">
          <h2>{group}</h2>
          {groupIndex === 0 && sortControl}
        </div>
        <FlexGrid gap="16px">
          {groupedProjects[group].map(([Component], idx) => (
            <div key={idx}>{Component}</div>
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
