import React from "react";
import "../../css/pages.css";

export default function IntegrationProject() {
  return (
    <>
      <div className="sectionDiv">
        {/* Description Section */}
        <section className="description-section">
          <h2 className="section-title">Course Overview</h2>
          <p>
            In this course, students work on a group project integrating and
            expanding on topics from various individual courses. Often called
            a "mini-bachelor," it serves as a precursor to the bachelor's project.
            Students apply knowledge in mobile/web applications, distributed and
            centralized solutions, graphics/game programming, AI, databases,
            cloud technologies, networking, and virtualization.
          </p>
          <p>
            Students also learn to apply agile development, project planning,
            requirements specification, design, implementation, testing,
            sustainability, information security, AI, and documentation.
          </p>
          <p>
            <a
              className="highlight-link"
              href="https://www.ntnu.edu/studies/courses/PROG2052#tab=omEmnett"
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
              This project was the first large team project of our studies, integrating multiple technologies into a cohesive product. I learned Scrum, React, connecting a website to a database, and efficient data retrieval. Working collaboratively provided a practical experience closer to real-world software development.
            </p>
          </div>

          <div className="tech-used">
            <h3 className="subsection-title">Technologies & Tools</h3>
            <ul className="custom-list">
              <li>Docker & Docker Compose</li>
              <li>Vite</li>
            </ul>

            <h3 className="subsection-title">Programming Languages</h3>
            <ul className="custom-list">
              <li>React.js</li>
              <li>Golang</li>
              <li>SQLite</li>
            </ul>
          </div>
        </section>

        {/* Project Section */}
        <section className="alternating-sections">
          <div className="row">
            <div className="text">
              <h3 className="section-title">Our Project</h3>
              <p>
                We developed a web application called <strong>FridgeIO</strong>,
                allowing users to track ingredients, monitor expiration dates,
                and reduce food waste. The app supports family or roommate collaboration
                and prioritizes mobile users for convenience in the kitchen or while shopping.
              </p>
            </div>
          </div>

          <div className="row">
            <div className="text">
              <h3 className="section-title">Project Process</h3>
              <p>
                A significant portion of the project focused on database structuring
                and wireframe design. We created an ERD for data relationships and a functional wireframe
                prioritizing usability. Limited time and experience meant some features could not
                be fully implemented, but the project provided a strong learning experience.
              </p>
              <p>
                <a
                  className="highlight-link"
                  href="https://github.com/Danielhoyland/Portfolio/tree/main/Intergrasjon%20-%20Full%20stack"
                  target="_blank"
                  rel="noopener noreferrer"
                >
                  View the project code on GitHub
                </a>
              </p>
            </div>
          </div>
        </section>
      </div>
    </>
  );
}
