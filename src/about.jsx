import React from "react";
import CircularText from "./components/CircularText";
import "./css/about.css";
import InterestsSection from "./components/InterestsSection";
import Portrett from "./images/DBrown.jpg";

const AboutMe = () => {
  return (
    <div className="about-container">
      <div className="about-row">
        {/* Left Side: Text */}
        <div className="about-text">
          <br></br>
          <h2 className="about-heading" id="about-me">
            About Me
          </h2>
          <p className="about-lead">
            I'm Daniel Høyland from Sandnes. I'm 24 years old and currently
            studying a Masters degree in Informatics: Software Engineering at NTNU
            Trondheim. I completed my bachelor's in Programming at NTNU
            Gjøvik.
          </p>
          <div className="interests">
            <InterestsSection
              interests={["D&D", "Chess", "Board Games", "Gaming", "Travel", "Cooking", "Bouldering", "Gym", "Running"]}
              Interval={7000}
              staggerDuration={0.04}
            />
            <InterestsSection
              interests={["ReactJs", "Golang", ".NET", "C", "C++", "JavaScript", "Haskell", "Rust", "Kotlin", "C#", "OpenGL", "Typescript/Tailwind", "NodeJS", "SQL", "Machine learning", "Angular","Docker", "Sever hosting"]}
              Interval={7000}
              staggerDuration={0.02}
            />
            <InterestsSection
              interests={[
                "Agile Development",
                "Web Development",
                "Graphical Programming",
                "Mobile Programming",
                "Cloud Development",
                "Database Management",
                "Game Development",
                "UX design"
              ]}
              Interval={7000}
              staggerDuration={0.04}
            />
          </div>
          <br></br>
          <br></br>
          <p className="about-paragraph">
            Throughout my studies, I've gained experience with many programming
            techniques, languages, and paradigms. As one of my professors put
            it:
            <br></br>
            <em>
              {" "}
              “We don't want you to just learn a programming language, we want
              you to learn how to code.”
            </em>
            <br></br>
            This philosophy has shaped the way I approach problems, focusing on
            writing clean, efficient, and adaptable code. I've worked with
            low-level languages like C and C++, which taught me transferable
            skills applicable to almost any programming language. <br />
            <br />
            I'm a problem-oriented programmer who enjoys tackling challenges
            that feel like puzzles. In my free time, I create apps to solve
            everyday problems, and I also enjoy hobbies like gaming, chess, gym,
            bouldering, and D&D. Chess has strengthened my problem-solving
            skills by training me to consider multiple possibilities before
            acting. D&D, where I'm often the Game Master, has boosted my
            creativity, requiring me to design scenarios, adapt quickly to
            unexpected player actions, and think outside the box. Both of these
            hobbies complement my work as a programmer, combining logic with
            creativity.
          </p>
        </div>

        {/* Right Side: Image with Circular Text */}
        <div className="about-image-wrapper">
          <div className="circular-image-container">
            <CircularText
              onHover="pause"
              text="Daniel*Høyland*"
              spinDuration={20}
              className="circular-top-left"
            />
            <img
              src={Portrett}
              className="profile-image"
              alt="Daniel Høyland"
            />
          </div>
        </div>
      </div>
    </div>
  );
};

export default AboutMe;
