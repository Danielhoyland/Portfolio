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
            This course focused on model-driven software engineering, with an
            emphasis on building modular and reusable systems using models,
            metamodels, and domain-specific languages (DSLs).
          </p>
          <p>
            The course explored software product lines, variability in software
            systems, and how code generation can be used to automate development
            and ensure consistency between design and implementation.
          </p>
          <p>
            <a
              className="highlight-link"
              href="https://www.ntnu.edu/studies/courses/TDT4250#tab=omEmnet"
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
              I gained hands-on experience with model-driven development and how
              to design flexible software systems using abstraction and
              automation. This included:
            </p>
            <ul className="custom-list">
              <li>Designing metamodels using Ecore</li>
              <li>Creating domain-specific languages (DSLs) with Xtext</li>
              <li>Implementing code generators using Xtend</li>
              <li>Applying software product line (SPL) principles</li>
              <li>Handling variability at design-time and runtime</li>
            </ul>
          </div>

          <div className="tech-used">
            <h3 className="subsection-title">Technologies & Tools</h3>
            <ul className="custom-list">
              <li>Eclipse Modeling Framework (EMF)</li>
              <li>Xtext</li>
              <li>Xtend</li>
              <li>Java & JavaFX</li>
              <li>Maven</li>
            </ul>
            <h3 className="subsection-title">Programming Languages</h3>
            <ul className="custom-list">
              <li>Java</li>
              <li>Xtend</li>
            </ul>
          </div>
        </section>

        {/* Examples Section */}
        <section className="alternating-sections">
          {/* Example 1 */}
          <div className="row">
            <div className="text">
              <h3 className="section-title">
                Event Pack DSL & Metamodel
              </h3>
              <p>
                The main project focused on designing a domain-specific language
                (DSL) and metamodel for creating "Event Packs" in a text-based
                game engine.
              </p>
              <p>
                The metamodel defined structured elements such as events,
                questions, and typing challenges, including constraints written
                in OCL to ensure valid model instances.
              </p>
              <p>
                I also customized the generated Xtext grammar to improve
                usability by removing redundancy and making the DSL more
                intuitive to write.
              </p>
            </div>
          </div>

          {/* Example 2 */}
          <div className="row">
            <div className="text">
              <h3 className="section-title">
                Code Generation & Automation
              </h3>
              <p>
                Using Xtend, we developed generators that transform DSL models
                into executable Java code and CSS. The generated code included
                full game logic for handling events, as well as dynamic UI
                styling based on event properties such as difficulty and type.
              </p>
              <p>
                Code generation was automatically triggered when editing DSL
                instances, ensuring synchronization between model definitions
                and implementation.
              </p>
              <p>
                This demonstrated how model-driven approaches can significantly
                reduce manual coding and improve consistency across systems.
              </p>
            </div>
          </div>

          {/* Example 3 */}
          <div className="row">
            <div className="text">
              <h3 className="section-title">
                System Integration & Architecture
              </h3>
              <p>
                The generated code was integrated into an existing Java-based
                game engine, where event packs could be executed as interactive
                mini-games.
              </p>
              <p>
                The project followed a modular architecture, separating the
                metamodel, DSL, generators, and runtime system. This allowed for
                flexible extension and future integration into the main game
                engine.
              </p>
              <p>
                A standalone event runner was also developed as a proof of
                concept, demonstrating how model-driven systems can be deployed
                independently while remaining compatible with larger systems.
              </p>
              <p>
                <a
                  className="highlight-link"
                  href="https://github.com/Danielhoyland/Portfolio/tree/main/projects/Model-Driven"
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