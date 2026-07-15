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
            The field of cloud computing and cloud technologies is dynamic and
            constantly evolving. While some fundamental concepts remain stable,
            tools and platforms often change rapidly. The course content is
            updated to reflect contemporary industry practices. Typical coverage
            includes:
          </p>
          <ul className="custom-list">
            <li>Architectural Types and Principles (IaaS, PaaS, SaaS)</li>
            <li>
              Cloud Technologies (Processing, Storage, Network, Virtualisation)
            </li>
            <li>
              API Standards (e.g., REST) and Standardisation Bodies (IETF)
            </li>
            <li>
              Cloud Vendors and Economics (Business models, Pricing,
              Service-level agreements)
            </li>
            <li>Cloud Operations Basics</li>
            <li>Development and Deployment of Cloud Applications</li>
          </ul>
          <p>
            <a
              className="highlight-link"
              href="https://www.ntnu.edu/studies/courses/PROG2005/#tab=omEmnet"
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
              In this course, I gained a deeper understanding of APIs and
              learned how to program in Golang, creating and consuming APIs to
              send and receive data efficiently.
            </p>
          </div>

          <div className="tech-used">
            <h3 className="subsection-title">Technologies & Tools</h3>
            <ul className="custom-list">
              <li>API development</li>
              <li>Firebase</li>
              <li>Postman</li>
            </ul>

            <h3 className="subsection-title">Programming Languages</h3>
            <ul className="custom-list">
              <li>Golang</li>
            </ul>
          </div>
        </section>

        {/* Project / Assignments Section */}
        <section className="alternating-sections">
          <div className="row">
            <div className="text">
              <h3 className="section-title">Assignment 1</h3>
              <p>
                The first assignment was done individually. I worked with two
                APIs related to countries and universities, which allowed
                queries like "NOR" to list all universities in Norway or find
                neighboring countries.
              </p>
            </div>
          </div>

          <div className="row">
            <div className="text">
              <h3 className="section-title">Assignment 2</h3>
              <p>
                The second assignment involved creating an API for a large CSV
                file containing climate data for many countries. The API allowed
                filtering by parameters such as country, date, CO2 emissions,
                and more.
              </p>
              <p>
                <a
                  className="highlight-link"
                  href="https://github.com/Danielhoyland/Portfolio/tree/main/projects/Kunstliginteligens"
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
