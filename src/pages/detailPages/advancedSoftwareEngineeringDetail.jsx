import React from "react";
import "../../css/pages.css";

export default function AdvancedSoftwareEngineeringDetail() {
  return (
    <>
      <div className="sectionDiv">
        {/* Description Section */}
        <section className="description-section">
          <h2 className="section-title">Course Overview</h2>
          <p>
            This course focused on advanced software engineering practices for
            smart, critical, and complex software-intensive systems. The course
            covered requirement specification, testing strategies, code quality,
            verification and validation, and the growing role of AI-assisted
            software engineering.
          </p>
          <p>
            Throughout the semester we worked on a group project where we
            developed an application that helps students determine whether their
            use of AI tools complies with university guidelines and submission
            requirements. The project followed an industry-inspired workflow,
            starting from requirement gathering and ending with testing and
            validation.
          </p>
          <p>
            <a
              className="highlight-link"
              href="https://www.ntnu.edu/studies/courses/TDT4240"
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
              This course gave me valuable experience in translating business
              requirements into technical solutions while also exploring how AI
              can assist the software development process. I gained hands-on
              experience with requirement engineering, prompt engineering,
              testing strategies, and validating that software meets both
              functional and non-functional requirements.
            </p>
            <ul className="custom-list">
              <li>
                Gathered requirements from a Product Owner using user stories
                and interviews
              </li>
              <li>
                Created functional and non-functional requirements from user
                needs
              </li>
              <li>
                Applied prompt engineering and AI-assisted development
                techniques
              </li>
              <li>
                Performed testing and validation to ensure requirements were
                fulfilled
              </li>
              <li>
                Learned how to critically evaluate AI-generated code and improve
                it through refactoring
              </li>
            </ul>
          </div>

          <div className="tech-used">
            <h3 className="subsection-title">Technologies & Tools</h3>
            <ul className="custom-list">
              <li>Git & GitHub</li>
              <li>AI-Assisted Development Tools</li>
              <li>Requirement Engineering Techniques</li>
              <li>Testing & Validation Methods</li>
              <li>Prompt Engineering</li>
            </ul>

            <h3 className="subsection-title">Programming Languages</h3>
            <ul className="custom-list">
              <li>JavaScript</li>
            </ul>
          </div>
        </section>

        {/* Examples Section */}
        <section className="alternating-sections">
          <div className="row">
            <div className="text">
              <h3 className="section-title">
                AI Guidelines Assistant Project
              </h3>
              <p>
                The main project in this course was to develop an application
                that acts as an AI guideline assistant for university students.
                The application helps students determine whether their use of AI
                tools complies with university regulations and assignment
                requirements.
              </p>

              <p>
                Our team first gathered information from a Product Owner using
                user stories and requirement elicitation techniques. From this,
                we created functional and non-functional requirements that
                defined the scope of the application.
              </p>

              <p>
                We then used prompt engineering and AI-assisted development to
                generate parts of the application before reviewing, refactoring,
                and improving the generated code ourselves. Finally, we tested
                the application and validated that it fulfilled the specified
                requirements to an acceptable degree.
              </p>

              <p>
                This project gave me valuable insight into how AI can be used as
                a development tool while still requiring traditional software
                engineering practices such as requirement analysis, testing, and
                quality assurance.
              </p>


              <p>
                <a
                  className="highlight-link"
                  href="https://github.com/Danielhoyland/Portfolio/tree/main/projects/Advance%20Software%20Engineering"
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