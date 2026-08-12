import React from "react";
import "../../css/pages.css";

export default function ScriptBlackboardDetail() {
  return (
    <>
      <div className="sectionDiv">
        {/* Description Section */}
        <section className="description-section">
          <h2 className="section-title">Project Overview</h2>
          <p>
            This personal project was created to simplify downloading course
            material from Blackboard. The platform only provided a manual
            download option where each course had to be opened individually and
            downloaded, which became inefficient when handling a large amount
            of files.
          </p>
          <p>
            I developed a Tampermonkey userscript that crawled through the
            course structure, identified available files, preserved the original
            folder organization, removed duplicate downloads, and generated a
            ZIP archive containing all course material.
          </p>
        </section>

        {/* Learning and Technology Section */}
        <section className="learning-tech-section">
          <div className="what-i-learned">
            <h2 className="section-title">Key Takeaways</h2>
            <p>
              This project gave me practical experience with browser
              automation, working with complex web applications, and handling
              challenges caused by dynamically generated content.
            </p>

            <ul className="custom-list">
              <li>
                Built a browser automation tool using JavaScript
              </li>
              <li>
                Learned how to inspect and navigate complex website structures
              </li>
              <li>
                Implemented recursive crawling and duplicate prevention logic
              </li>
            </ul>
          </div>

          <div className="tech-used">
            <h3 className="subsection-title">Technologies & Tools</h3>
            <ul className="custom-list">
              <li>Tampermonkey</li>
              <li>Browser APIs</li>
              <li>ZIP file generation</li>
              <li>DOM manipulation</li>
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
              <h3 className="section-title">Project Details</h3>

              <p>
                The project started from the need to archive course material
                from Blackboard in a more efficient way. Instead of manually
                navigating through each folder and downloading files one by one,
                I created a browser script that could automatically discover
                and collect available content.
              </p>

              <p>
                The script was implemented as a Tampermonkey userscript, which
                allowed it to run directly inside the Blackboard webpage. The
                script added custom buttons to the interface, making the tool
                accessible without requiring users to open developer tools or
                manually execute commands.
              </p>

              <p>
                The main challenge was understanding Blackboard's dynamically
                generated course structure. The script needed to navigate
                through different sections, identify files, and follow the same
                organization used by the course page while avoiding downloading
                duplicate files.
              </p>

              <p>
                To solve this, I implemented crawling logic that mapped the
                course structure recursively and tracked already processed
                files. The collected files were then organized into folders and
                compressed into a ZIP archive before being downloaded locally.
              </p>

              <p>
                The project also required handling limitations caused by
                Blackboard's web architecture, including dynamically loaded
                content and restrictions when accessing external file resources.
                Solving these issues improved my understanding of how modern
                web applications communicate and how browser automation tools
                interact with them.
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