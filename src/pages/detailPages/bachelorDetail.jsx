import React from "react";
import "../../css/pages.css";

export default function AlbumDetails() {
  return (
    <>
      <div className="sectionDiv">
        {/* Description Section */}
        <section className="description-section">
          <h2 className="section-title">Course Overview</h2>
          <p>
            The bachelor thesis is based on either a problem/thesis proposal
            obtained from business, public sector, or the academic community.
            The proposals must be pre-approved by the course coordinator.
            Through the assignment, students will identify, formulate, and solve
            relevant problems within programmers' field of work.
          </p>
          <p>
            The candidate must use knowledge and skills from several subject
            areas in the study, as well as independent specialization where
            necessary. The bachelor thesis contributes to either research or
            development, and the candidate must acquire skills in project
            management, planning, implementation, and documentation.
          </p>
          <p>
            The project must include software development, and may also include
            elements of research, innovation, or entrepreneurship.
          </p>
          <p>
            <a
              className="highlight-link"
              href="https://www.ntnu.edu/studies/courses/PROG2900/#tab=omEmnet"
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
              Me and my group got a task from Innoveria. Our task was to create
              a web application that collects, stores, and displays electrical
              usage from sensors placed on machines in a factory. The
              application should be available on PC and tablet, deliver an
              intuitive user experience, and be scalable for future development.
            </p>
            <p>The application included the following requirements:</p>
            <ul className="custom-list">
              <li>
                Overview page with multiple views:
                <ul className="custom-list">
                  <li>Graph view</li>
                  <li>Table view</li>
                  <li>
                    Layered sorting (Building → Department → Production line →
                    Machine)
                  </li>
                  <li>
                    Filtering by layer, machine, highest consumer, or time
                  </li>
                </ul>
              </li>
              <li>Access rights and permissions</li>
              <li>
                ENØK measures:
                <ul className="custom-list">
                  <li>Accepted/proposed measures</li>
                  <li>
                    Superuser permissions to accept/discard proposed measures
                  </li>
                </ul>
              </li>
              <li>Adding/removing sensors and gateways</li>
            </ul>
          </div>

          <div className="tech-used">
            <h3 className="subsection-title">Technologies & Tools</h3>
            <ul className="custom-list">
              <li>Docker & Docker Compose</li>
              <li>ShadCN</li>
              <li>Chirpstack</li>
              <li>Vite</li>
              <li>LoRaWAN</li>
              <li>
                <a
                  className="highlight-link"
                  href="https://vutility.com/hotdrop"
                  target="_blank"
                  rel="noopener noreferrer"
                >
                  Vutility HotDrop
                </a>
              </li>
              <li>
                <a
                  className="highlight-link"
                  href="https://store.rakwireless.com/products/rak7268-8-channel-indoor-lorawan-gateway?m=3&h=wisgate-edge&variant=42316476678342"
                  target="_blank"
                  rel="noopener noreferrer"
                >
                  WisGate Edge Lite 2
                </a>
              </li>
            </ul>

            <h3 className="subsection-title">Programming Languages</h3>
            <ul className="custom-list">
              <li>React.js</li>
              <li>Golang</li>
              <li>MySQL</li>
            </ul>
          </div>
        </section>
        <div className="row">
          <div className="text">
            <h3 className="section-title">What I Learned</h3>
            <p>
              I gained hands-on experience in structuring databases, automating
              data processing and storage, and developing with React.js.
            </p>
            <p>
              I also learned to apply Scrum methodology in a real-world setting,
              improving my client communication skills and adapting the project
              based on feedback.
            </p>
          </div>
        </div>
        {/* Project Details Section */}
        <section className="alternating-sections">
          <div className="row">
            <div className="text">
              <h3 className="section-title">The Project</h3>
              <p>
                The project was divided into three main components: Frontend
                (design), Backend (APIs and database), and the LoRaWAN server
                (data connection and collection).
              </p>
              <p>
                We applied Scrum as an agile methodology throughout the project.
                The system integrated the database, the web application, and Rak
                gateways through APIs, automatically collecting, processing, and
                storing data from factory sensors for near real-time monitoring.
              </p>
              <p>
                <a
                  className="highlight-link"
                  href="https://ntnuopen.ntnu.no/ntnu-xmlui/handle/11250/3137511"
                  target="_blank"
                  rel="noopener noreferrer"
                >
                  View the full report
                </a>
              </p>
              <p>
                <a
                  className="highlight-link"
                  href="https://github.com/Danielhoyland/Portfolio/tree/main/projects/Bachelor%20-%20Full%20stack"
                  target="_blank"
                  rel="noopener noreferrer"
                >
                  View the project on GitHub
                </a>
              </p>
            </div>
          </div>
        </section>
      </div>
    </>
  );
}
