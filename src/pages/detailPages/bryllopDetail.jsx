import React from "react";
import "../../css/pages.css";

export default function BryllopDetail() {
  return (
    <>
      <div className="sectionDiv">
        {/* Description Section */}
        <section className="description-section">
          <h2 className="section-title">Project Overview</h2>
          <p>
            My sister wanted a website for both PC and Phones where one could
            see the wedding program, the food menu, the seating map and find
            easily where one self sits and song texts. She wanted it in the
            style of the wedding invitation in addition to be a website that
            didn't cost any money to host.
          </p>
        </section>

        {/* Learning and Technology Section */}
        <section className="learning-tech-section">
          <div className="tech-used">
            <h3 className="subsection-title">Technologies & Tools</h3>
            <ul className="custom-list">
              <li>github-pages</li>
            </ul>
            <h3 className="subsection-title">Programming Languages</h3>
            <ul className="custom-list">
              <li>React</li>
            </ul>
          </div>
        </section>

        {/* Examples Section */}
        <section className="alternating-sections">
          <div className="row">
            <div className="text">
              <h3 className="section-title">Project Details</h3>
              <p>
                I started by making a template based on the wedding invitation
                with the color themes and the style with lines and text fonts,
                in addition to add a title that was fitting for a wedding. This
                was done in react.
              </p>
              <p>
                After that I set up a workflow to automatically push changes
                from main to github-pages and tinker with the URL to not having
                "github" in the URL. Since it cost money with a domain the URL
                stayed unchanged.
              </p>
              <p>
                After that I waited on the data she wanted in and added it when
                i got it, where my focus was on usability and styling both on
                desktop and especially on a phone view. Additional functionality
                was added, like scrolling on song text, navigation and a custom
                navigation for the seating map.
              </p>
              <p>
                <a
                  className="highlight-link"
                  href="https://github.com/Danielhoyland/Portfolio/tree/main/projects/Bryllup"
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
