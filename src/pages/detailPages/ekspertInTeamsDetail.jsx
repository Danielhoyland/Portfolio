import React from "react";
import "../../css/pages.css";

export default function EkspertInTeamsDetail() {
  return (
    <>
      <div className="sectionDiv">
        {/* Description Section */}
        <section className="description-section">
          <h2 className="section-title">Project Overview</h2>
          <p>
            Experts in Teamwork (Eksperter i Team) is NTNU's interdisciplinary
            collaboration course, where students from different fields work
            together to solve real-world challenges while developing teamwork
            and communication skills.
          </p>
          <p>
            Our group explored the theme <em>Space – Satellites and Mars
            Colonization</em> by designing an autonomous plant pod capable of
            sustaining plant growth under Martian conditions. My primary
            responsibility was developing the full-stack monitoring system,
            enabling sensor data to be collected, stored, visualized, and used
            for remote control of the prototype.
          </p>
        </section>

        {/* Learning and Technology Section */}
        <section className="learning-tech-section">
          <div className="what-i-learned">
            <h2 className="section-title">Key Takeaways</h2>

            <p>
              This project strengthened both my technical and collaborative
              skills. Working alongside students from biotechnology,
              mechanical engineering, cybernetics, and computer science taught
              me how software integrates with hardware and scientific research
              to solve complex engineering problems.
            </p>

            <ul className="custom-list">
              <li>Designed and developed a full-stack monitoring platform</li>
              <li>Collaborated in a multidisciplinary engineering team</li>
              <li>Integrated hardware telemetry with a web dashboard</li>
            </ul>
          </div>

          <div className="tech-used">
            <h3 className="subsection-title">Technologies & Tools</h3>
            <ul className="custom-list">
              <li>Raspberry Pi</li>
              <li>REST API</li>
              <li>MySQL</li>
              <li>GitHub Actions</li>
              <li>Linux (Ubuntu)</li>
              <li>3D Printing</li>
            </ul>

            <h3 className="subsection-title">Programming Languages</h3>
            <ul className="custom-list">
              <li>Go</li>
              <li>React</li>
              <li>Python</li>
            </ul>
          </div>
        </section>

        {/* Examples Section */}
        <section className="alternating-sections">
          <div className="row">
            <div className="text">
              <h3 className="section-title">Project Details</h3>

              <p>
                Our objective was to investigate whether a self-sufficient plant
                pod could support plant growth during a future Mars mission. The
                project combined biological research, mechanical design,
                electronics, autonomous control, and software engineering into
                a single proof-of-concept prototype.
              </p>

              <p>
                My main responsibility was the software infrastructure. I
                designed and implemented a full-stack application consisting of
                a Go backend, a MySQL database, and a React dashboard hosted on
                NTNU's Skyhigh server. The backend exposed a REST API that
                received telemetry from a Raspberry Pi, stored sensor readings,
                and delivered live data to the dashboard.
              </p>

              <p>
                The Raspberry Pi acted as the autonomous controller inside the
                plant pod, collecting measurements such as temperature,
                humidity, pressure, CO₂ concentration, soil moisture, and light
                levels. The dashboard allowed these values to be monitored in
                real time while also providing an interface for updating target
                environmental conditions that were sent back to the controller.
              </p>

              <p>
                Beyond implementation, I focused on designing the architecture
                to resemble a realistic Mars deployment. The system stored
                telemetry using measurement timestamps rather than arrival
                times, making it robust against delayed or out-of-order data.
                We also discussed communication strategies suitable for
                interplanetary missions, including autonomous edge processing,
                delay-tolerant communication principles, and secure command
                handling.
              </p>

              <p>
                Working with teammates from biotechnology, robotics, mechanical
                engineering, and computer science was one of the most rewarding
                aspects of the project. It demonstrated how software engineering
                plays a central role in connecting sensors, hardware, biology,
                and user interfaces into a complete engineering solution.
              </p>

              {/* Add report or GitHub link if public */}
              /* <p>
                <a
                  className="highlight-link"
                  href="https://github.com/Danielhoyland/Portfolio/tree/main/projects/Experts%20in%20Teams"
                  target="_blank"
                  rel="noopener noreferrer"
                >
                  View the project
                </a>
              </p> */
            </div>
          </div>
        </section>
      </div>
    </>
  );
}