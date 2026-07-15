import React from "react";
import "../../css/pages.css";

export default function DatabaseSystems() {
  return (
    <>
      <div className="sectionDiv">
        {/* Description Section */}
        <section className="description-section">
          <h2 className="section-title">Course Overview</h2>
          <p>
            This course introduced the fundamentals of data modeling and
            database systems. Topics included:
          </p>
          <ul className="custom-list">
            <li>
              Basic introduction to data modeling with emphasis on conceptual
              modeling
            </li>
            <li>The relational model, relational algebra, and SQL</li>
            <li>Normalization as a design theory for relational databases</li>
            <li>Other database models</li>
            <li>Database construction and development</li>
            <li>Testing database applications</li>
            <li>Security in database systems</li>
            <li>
              Storage technologies, file organization, and index structures
            </li>
            <li>Query processing</li>
            <li>Database management systems (DBMS)</li>
            <li>Data integrity, transactions, concurrency, and recovery</li>
          </ul>
          <p>
            <a
              className="highlight-link"
              href="https://www.ntnu.edu/studies/courses/IDATG2204/#tab=omEmnet"
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
              I learned how to plan, create, and manage effective databases. I
              also gained experience testing databases using Python.
            </p>
          </div>

          <div className="tech-used">
            <h3 className="subsection-title">Technologies & Tools</h3>
            <ul className="custom-list">
              <li>MySQL</li>
              <li>MariaDB</li>
              <li>NoSQL</li>
            </ul>

            <h3 className="subsection-title">Programming Languages</h3>
            <ul className="custom-list">
              <li>SQL</li>
              <li>Python</li>
            </ul>
          </div>
        </section>

        {/* Project / Assignments Section */}
        <section className="alternating-sections">
          <div className="row">
            <div className="text">
              <h3 className="section-title">Course Work</h3>
              <p>
                During the course, we performed numerous SQL queries to
                retrieve, sort, and insert data. We also created an ERD up to
                BCNF normal form, constructed and populated the database, and
                tested functional connections between tables using Python.
              </p>
            </div>
          </div>

          <div className="row">
            <div className="text">
              <h3 className="section-title">Project</h3>
              <p>
                The final project involved designing a database for teachers and
                students, with different permission levels for booking rooms in
                various buildings, and storing relevant information.
              </p>
              <p>
                <a
                  className="highlight-link"
                  href="https://github.com/Danielhoyland/Portfolio/tree/main/projects/Database"
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
