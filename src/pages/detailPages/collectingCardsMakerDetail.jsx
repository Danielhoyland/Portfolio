import React from "react";
import "../../css/pages.css";

export default function CollectingCardsMakerDetail() {
  return (
    <>
      <div className="sectionDiv">
        {/* Description Section */}
        <section className="description-section">
          <h2 className="section-title">Project Overview</h2>
          <p>
            This project was created to automate the production of custom
            trading-style cards for my siblings' weddings. The goal was to
            process hundreds of images with consistent dimensions while applying
            different decorative frames based on simple file naming
            conventions.
          </p>
          <p>
            Instead of manually editing every image, the application resized,
            cropped, and combined photos with custom borders, significantly
            reducing the time required to generate the final cards.
          </p>
        </section>

        {/* Learning and Technology Section */}
        <section className="learning-tech-section">
          <div className="what-i-learned">
            <h2 className="section-title">Key Takeaways</h2>
            <p>
              This project taught me how to automate repetitive image
              processing tasks while balancing automation with practical manual
              adjustments. It also gave me experience solving problems where
              there was no single perfect solution.
            </p>

            <ul className="custom-list">
              <li>Automated batch image processing</li>
              <li>Designed a flexible naming convention for customization</li>
              <li>Explored multiple approaches to automatic image cropping</li>
            </ul>
          </div>

          <div className="tech-used">
            <h3 className="subsection-title">Technologies & Tools</h3>
            <ul className="custom-list">
              <li>Pillow (PIL)</li>
              <li>File System Automation</li>
            </ul>

            <h3 className="subsection-title">Programming Languages</h3>
            <ul className="custom-list">
              <li>Python</li>
            </ul>
          </div>
        </section>

        {/* Examples Section */}
        <section className="alternating-sections">
          <div className="row">
            <div className="text">
              <h3 className="section-title">Project Details</h3>

              <p>
                The project began with creating custom card templates and
                testing how decorative borders could be merged with individual
                photos. Once the basic process worked, I expanded the prototype
                into a batch-processing tool capable of converting every image
                inside a folder and exporting the finished cards to a separate
                directory.
              </p>

              <p>
                The biggest challenge was cropping images automatically. Since
                photos varied greatly in composition, a simple centered crop
                often produced poor results by cutting off faces or placing the
                subject too close to the edge of the card.
              </p>

              <p>
                I explored several possible solutions:
              </p>

              <ul className="custom-list">
                <li>Automatic cropping using facial recognition</li>
                <li>
                  Directional file naming (N, S, E, W, and C) to control crop
                  positioning
                </li>
                <li>Using the full image instead of cropped sections</li>
              </ul>

              <p>
                While each approach worked in certain situations, none provided
                consistently good results. I ultimately chose a hybrid
                solution—allowing the software to process most images
                automatically while manually adjusting only the few that
                required additional attention.
              </p>

              <p>
                To make customization even easier, I implemented support for
                multiple border designs. The application selected the
                appropriate frame simply by reading a number prefixed to the
                filename, allowing hundreds of cards with different designs to
                be generated in a single run without modifying the code.
              </p>

              <p>
                <a
                  className="highlight-link"
                  href="https://github.com/Danielhoyland/Portfolio/tree/main/projects/collecting-cards"
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