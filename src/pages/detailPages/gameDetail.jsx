import React from "react";
import "../../css/pages.css";

export default function GameProgramming() {
  return (
    <>
      <div className="sectionDiv">
        {/* Description Section */}
        <section className="description-section">
          <h2 className="section-title">Course Overview</h2>
          <p>
            This course covered essential knowledge for students to advance in
            game development using modern technology. Topics included:
          </p>
          <ul className="custom-list">
            <li>Design patterns</li>
            <li>Graphics</li>
            <li>Physics in games</li>
            <li>Character development</li>
            <li>Animation</li>
            <li>Game-specific AI</li>
            <li>Implementing game mechanics</li>
            <li>Game production process</li>
            <li>Project management in teams</li>
            <li>Memory management</li>
            <li>Algorithm efficiency</li>
            <li>Advanced programming techniques</li>
          </ul>
          <p>
            <a
              className="highlight-link"
              href="https://www.ntnu.edu/studies/courses/IMT3603#tab=omEmnet"
              target="_blank"
              rel="noopener noreferrer"
            >
              View official course page
            </a>
          </p>
        </section>

        {/* Learning and Technology Section */}
        <section className="learning-tech-section">
          <div className="what-i-learned">
            <h2 className="section-title">Key Takeaways</h2>
            <p>
              I gained experience with animation, event-driven function
              handling, control mechanisms, and UI design to enhance user
              experience in video games. Most of my learning was specific to
              game programming using Unity, making transitions to other engines
              more challenging.
            </p>
          </div>

          <div className="tech-used">
            <h3 className="subsection-title">Technologies & Tools</h3>
            <ul className="custom-list">
              <li>Unity</li>
            </ul>

            <h3 className="subsection-title">Programming Languages</h3>
            <ul className="custom-list">
              <li>C#</li>
            </ul>
          </div>
        </section>

        {/* Project / Assignments Section */}
        <section className="alternating-sections">
          <div className="row">
            <div className="text">
              <h3 className="section-title">The Game Idea</h3>
              <p>
                The project involved creating a running game where the player
                tries to reach the finish line in various levels, with a unique
                mechanic that allows the player to throw a ball and teleport to
                its location. We experimented with controls, camera movements,
                camera shake, sound effects, and particle effects.
              </p>
            </div>
          </div>

          <div className="row">
            <div className="text">
              <h3 className="section-title">The Game</h3>
              <p>The finished game demonstrates the gameplay and mechanics.</p>
              <p>
                <a
                  className="highlight-link"
                  href="https://github.com/Danielhoyland/Portfolio/tree/main/projects/Gameprog%20-%20c%23%20Unity"
                  target="_blank"
                  rel="noopener noreferrer"
                >
                  View the project on GitHub
                </a>
              </p>
              <p>
                <a
                  className="highlight-link"
                  href="./src/download/AcrobaticBall2D.zip"
                  download
                >
                  Download game
                </a>
              </p>
            </div>
          </div>
        </section>
      </div>
    </>
  );
}
