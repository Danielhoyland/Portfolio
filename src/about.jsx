import React from 'react';
import CircularText from './components/CircularText';
import '../style.css'; // Your own CSS
import InterestsSection from './components/InterestsSection';

const AboutMe = () => {
  const myInterests = [
    'Web Development',
    'AI & Machine Learning',
    'Game Design',
    'Creative Coding',
    'Open Source',
    'Problem Solving',
  ];
  
  return (
    <div className="about-container">
      <div className="about-row">
        {/* Left Side: Text */}
        <div className="about-text">
          <br></br>
          <h2 className="about-heading" id="about-me">About Me</h2>
          <p className="about-lead">
            I'm Daniel Høyland from Sandnes, now based in Trondheim. I recently completed my bachelor's in Programming
            at NTNU Gjøvik. I'm passionate about software development, continuous learning, and solving meaningful
            problems with code.
          </p>
        <InterestsSection interests={["Learning", "Coding", "Improving", "Repeat"]} Interval={2500} />
        <InterestsSection interests={["Board Games", "Gaming", "Lego", "Travel"]} Interval={2400} />
        <InterestsSection interests={["Bouldering", "D&D", "Gym", "Chess"]} Interval={2600} />
         <br></br>
          <br></br>
          <p className="about-paragraph">
            I love diving into new tech, building personal projects, and experimenting with ways to blend creativity and logic — 
            whether it’s game development, system design, or web technologies.
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
              src="PortfolioWebsite/images/DBrown.jpg"
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
