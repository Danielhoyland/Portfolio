import React from "react";
import "../../css/pages.css";

export default function MobileProgramming() {
  return (
    <>
      <div className="sectionDiv">
        {/* Description Section */}
        <section className="description-section">
          <h2 className="section-title">Course Overview</h2>
          <p>
            The field of mobile system development changes rapidly. Topics
            covered include strengths and weaknesses of mobile technologies,
            limitations of devices, design patterns, layout and UI design,
            sensor integration, OS-specific development issues, networking, and
            cloud technology. Advanced optional topics may include raw data
            filtering, signal processing, and IoT.
          </p>
        </section>

        {/* Learning and Technology Section */}
        <section className="learning-tech-section">
          <div className="what-i-learned">
            <h2 className="section-title">Key Takeaways</h2>
            <p>
              I learned Kotlin and how to develop Android mobile applications.
              I also gained knowledge in mobile UI design best practices and
              effectively integrating mobile sensors and functionalities.
            </p>
          </div>

          <div className="tech-used">
            <h3 className="subsection-title">Technologies & Tools</h3>
            <ul className="custom-list">
              <li>Android Studios</li>
            </ul>

            <h3 className="subsection-title">Programming Languages</h3>
            <ul className="custom-list">
              <li>Kotlin</li>
            </ul>
          </div>
        </section>

        {/* Project Section */}
        <section className="alternating-sections">
          <div className="row">
            <div className="text">
              <h3 className="section-title">Project in Mobile Programming</h3>
              <p>
                Our main project was <strong>InkReader</strong>, a mobile app
                designed to track book reading and encourage users to read more.
                Features included a calendar to track pages read, adjustable
                reading goals, and an aquarium visualization that fills with
                fish based on reading streaks.
              </p>
              <p>
                <a
                  className="highlight-link"
                  href="https://github.com/Danielhoyland/Portfolio/tree/main/Mobile%20-%20Kotlin/MobileProject"
                  target="_blank"
                  rel="noopener noreferrer"
                >
                  View the project code on GitHub
                </a>
              </p>
            </div>
          </div>

          <div className="row">
            <div className="text">
              <h3 className="section-title">Personal Projects and Post-Course Learning</h3>
              <p>
                After the course, I developed several personal projects:
                <ul>
                  <li>
                    A quote app to save and filter quotes, with a drinking game
                    feature.
                  </li>
                  <li>
                    A tabletop game companion app to streamline game-related
                    tasks previously done manually.
                  </li>
                </ul>
                These projects enhanced my understanding of mobile development,
                gestures, and data management. I also started learning React
                Native to create apps for both Android and iOS efficiently.
              </p>
            </div>
          </div>
        </section>
      </div>
    </>
  );
}
