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
            This project was to make collecting cards in a fast way with custom
            dimensions on different sized pictures with multiple frames around
            the different pictures for a huge amount of pictures. This was done
            by semi automatic resizing of images and a naming system to choose
            what frame goes on what picture.
          </p>
        </section>

        {/* Learning and Technology Section */}
        <section className="learning-tech-section">
          <div className="what-i-learned"></div>

          <div className="tech-used">
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
                I wanted to make collecting cards for both my siblings weddings
                so I made a as automatically program as possible to convert
                different images to collecting cards of the same size. I began
                by collecting cards and made custom temporary frames. Then I
                made a mini prototype to add the frame to a image. When I
                managed to do that I tried to automatically to do it to every
                image in a folder and then output them into a different folder.
              </p>
              <p>
                A problem I got after that was that the border either was to
                wide, meaning I wanted a more zoomed in picture or it didnt get
                what I wanted, meaning the faces in the picture could be on the
                left side of the image but the border used the height of the
                image and centered afterwards so it didnt show anything outside
                the image.
              </p>
              <p>I tried to fix this in multiple ways:</p>
              <ul>
                <li>Automatic placement based on facial reconition</li>
                <li>
                  Directional naming: Adding N S W E C for NORTH SOUTH WEST EAST
                  or CENTER in the name of the file
                </li>
                <li>Use the whole image instead of a cutout</li>
              </ul>
              <p>
                All of these either was to complex, inefficient or/and gave bad
                results. In the end I needed to manually cut down some images
                while most I could leave untouched.
              </p>
              <p>
                After this I added a border folder where, one can add border 1
                to X. In the naming of the picture would decide the border type
                it would have, so a picture called "Smile.png" would be changed
                to for example "2 Smile.png" and then it would get border 2.
              </p>
              <p>
                <a
                  className="highlight-link"
                  href="https://github.com/Danielhoyland/Portfolio/tree/main/projects/collectingCards"
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
