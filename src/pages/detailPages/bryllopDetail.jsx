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
            My sister wanted a wedding website that worked well on both desktop
            and mobile devices. The website needed to display the wedding
            program, menu, seating arrangements, song lyrics, and make it easy
            for guests to find where they were seated.
          </p>
          <p>
            She also wanted the website to match the style of the wedding
            invitation while remaining completely free to host.
          </p>
        </section>

        {/* Learning and Technology Section */}
        <section className="learning-tech-section">
          <div className="what-i-learned">
            <h2 className="section-title">Key Takeaways</h2>
            <p>
              This project gave me experience designing a website for
              non-technical users with a strong focus on usability, responsive
              design, and creating a polished visual experience that matched an
              existing design language.
            </p>
            <ul className="custom-list">
              <li>Designed a responsive user interface for mobile and desktop</li>
              <li>Created a consistent design based on an existing invitation</li>
              <li>Configured automatic deployment using GitHub Pages</li>
            </ul>
          </div>

          <div className="tech-used">
            <h3 className="subsection-title">Technologies & Tools</h3>
            <ul className="custom-list">
              <li>React</li>
              <li>GitHub Pages</li>
              <li>GitHub Actions</li>
            </ul>

            <h3 className="subsection-title">Programming Languages</h3>
            <ul className="custom-list">
              <li>JavaScript</li>
              <li>HTML</li>
              <li>CSS</li>
            </ul>
          </div>
        </section>

        {/* Examples Section */}
        <section className="alternating-sections">
          <div className="row">
            <div className="text">
              <h3 className="section-title">Project Details</h3>

              <p>
                I began by creating a template inspired by the wedding
                invitation, recreating its colors, typography, and decorative
                elements to provide a consistent visual identity. The entire
                interface was built using React.
              </p>

              <p>
                Once the design was complete, I configured automatic deployment
                with GitHub Pages so every change pushed to the main branch was
                published automatically. I also explored using a custom domain,
                but decided against it since the project was intended to remain
                completely free to host.
              </p>

              <p>
                As the wedding details became available, I gradually added new
                content while keeping usability as the main priority. The final
                website included responsive navigation, searchable seating
                arrangements, song lyrics with smooth scrolling, and layouts
                optimized for both desktop and mobile devices.
              </p>

              <p>
                <a
                  className="highlight-link"
                  href="https://github.com/Danielhoyland/Portfolio/tree/main/projects/Bryllup"
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