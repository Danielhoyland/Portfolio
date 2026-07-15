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
            Add project description here.
          </p>
        </section>

        {/* Learning and Technology Section */}
        <section className="learning-tech-section">
          <div className="what-i-learned">
            <h2 className="section-title">Key Takeaways</h2>
            <p>
              Add key learnings here.
            </p>
            <ul className="custom-list">
              <li>Add learning point 1</li>
              <li>Add learning point 2</li>
              <li>Add learning point 3</li>
            </ul>
          </div>

          <div className="tech-used">
            <h3 className="subsection-title">Technologies & Tools</h3>
            <ul className="custom-list">
              <li>Add technology 1</li>
            </ul>
            <h3 className="subsection-title">Programming Languages</h3>
            <ul className="custom-list">
              <li>Add language 1</li>
            </ul>
          </div>
        </section>

        {/* Examples Section */}
        <section className="alternating-sections">
          <div className="row">
            <div className="text">
              <h3 className="section-title">Project Details</h3>
              <p>
                Add project details and description here.
              </p>
              <p>
                <a
                  className="highlight-link"
                  href="#"
                  target="_blank"
                  rel="noopener noreferrer"
                >
                  View on GitHub
                </a>
              </p>
            </div>
          </div>
        </section>
      </div>
    </>
  );
}
