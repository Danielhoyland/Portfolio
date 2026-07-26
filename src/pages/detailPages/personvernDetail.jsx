import React from "react";
import "../../css/pages.css";

export default function PersonvernDetail() {
  return (
    <>
      <div className="sectionDiv">
        {/* Description Section */}
        <section className="description-section">
          <h2 className="section-title">Course Overview</h2>
          <p>
            This course introduced the principles of secure software
            development and data privacy, with a focus on identifying,
            preventing, and mitigating security vulnerabilities in web
            applications.
          </p>
          <p>
            Throughout the course, I worked with practical security exercises
            covering vulnerability discovery, exploitation in controlled
            environments, secure implementation, threat modeling, and risk
            assessment. The goal was to understand how security should be
            integrated throughout the entire software development lifecycle.
          </p>
        </section>

        {/* Learning and Technology Section */}
        <section className="learning-tech-section">
          <div className="what-i-learned">
            <h2 className="section-title">Key Takeaways</h2>
            <p>
              This course gave me a deeper understanding of how software
              vulnerabilities occur and how security should be considered from
              the early design stages through implementation and testing.
            </p>

            <ul className="custom-list">
              <li>
                Identified and analyzed common web application vulnerabilities
              </li>
              <li>
                Applied threat modeling and risk assessment techniques
              </li>
              <li>
                Learned secure software development practices and testing
                methods
              </li>
            </ul>
          </div>

          <div className="tech-used">
            <h3 className="subsection-title">Technologies & Tools</h3>
            <ul className="custom-list">
              <li>OWASP Top 10</li>
              <li>Threat Dragon</li>
              <li>Security Testing Tools</li>
              <li>Git</li>
            </ul>

            <h3 className="subsection-title">Programming Languages</h3>
            <ul className="custom-list">
              <li>Python</li>
              <li>JavaScript</li>
            </ul>
          </div>
        </section>

        {/* Examples Section */}
        <section className="alternating-sections">
          <div className="row">
            <div className="text">
              <h3 className="section-title">Course Details</h3>

              <p>
                The first assignment focused on discovering security
                vulnerabilities in a provided web application. I performed both
                white-box and black-box security analysis, identifying common
                vulnerabilities and documenting how they could be exploited in a
                controlled testing environment.
              </p>

              <p>
                The vulnerabilities were analyzed using principles from the
                OWASP Top 10, which provided a structured approach for
                understanding common web security issues. For each vulnerability,
                I investigated why it occurred, how it could be exploited, and
                what impact it could have on the application.
              </p>

              <p>
                The second assignment focused on improving the security of the
                application by fixing the identified vulnerabilities. This
                provided practical experience with secure coding practices and
                demonstrated how security improvements affect the development
                process.
              </p>

              <p>
                The final assignment focused on threat modeling and risk
                assessment. Based on a given scenario, I analyzed possible
                security threats, evaluated their potential impact and
                likelihood, and used threat modeling techniques to identify
                appropriate countermeasures.
              </p>

              <p>
                Through this course, I gained a better understanding of security
                as an ongoing part of software development rather than something
                added after implementation. I learned how developers can use
                structured methods to identify risks, design safer systems, and
                reduce vulnerabilities before they reach production.
              </p>

              <p>
                <a
                  className="highlight-link"
                  href="#"
                  target="_blank"
                  rel="noopener noreferrer"
                >
                  View course assignments on GitHub
                </a>
              </p>
            </div>
          </div>
        </section>
      </div>
    </>
  );
}