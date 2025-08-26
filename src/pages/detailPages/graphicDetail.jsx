import React from "react";
import "../../css/pages.css";

export default function GraphicsProgramming() {
  return (
    <>
      <div className="sectionDiv">
        {/* Description Section */}
        <section className="description-section">
          <h2 className="section-title">Course Overview</h2>
          <p>This course focused on graphics programming, covering:</p>
          <ul className="custom-list">
            <li>OpenGL Graphics Pipeline (2D/3D)</li>
            <li>Geometric Transformations (2D/3D)</li>
            <li>Colors, Textures, and Blending</li>
            <li>Shader Programming with GLSL</li>
            <li>Illumination Techniques</li>
            <li>C++ Programming with CMake and Git</li>
          </ul>
          <p>
            <a
              className="highlight-link"
              href="https://www.ntnu.edu/studies/courses/PROG2002#tab=omEmnet"
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
              This subject emphasized low-level programming. Initially challenging, I gained extensive knowledge in object creation, camera movements, rendering, shading, texturing, lighting, and memory management, including preventing memory leaks.
            </p>
          </div>

          <div className="tech-used">
            <h3 className="subsection-title">Technologies & Tools</h3>
            <ul className="custom-list">
              <li>CMake</li>
              <li>OpenGL</li>
              <li>GLFW</li>
              <li>GLM</li>
              <li>GLAD</li>
              <li>tclap</li>
              <li>stb</li>
            </ul>

            <h3 className="subsection-title">Programming Languages</h3>
            <ul className="custom-list">
              <li>C++</li>
            </ul>
          </div>
        </section>

        {/* Projects Section */}
        <section className="alternating-sections">
          <div className="row">
            <div className="text">
              <h3 className="section-title">Project 1</h3>
              <p>
                In the first project, we programmed using OpenGL and CMake to create graphical boxes with textures and simulate camera movement. The project culminated in a chessboard with multiple textured objects and movable pieces. I learned from initial texture mapping mistakes and improved in subsequent projects.
              </p>
              <p>
                <em>Disclaimer</em>: Some demo videos may appear laggy due to GIF limitations, but higher-quality versions are included in the project code.
              </p>
            </div>
          </div>

          <div className="row">
            <div className="text">
              <h3 className="section-title">Project 2</h3>
              <p>
                The second project was a three-day exam creating a Sokoban-like 3D game. Using knowledge from Project 1, I applied textures, lighting, and object manipulation to build the game. Challenges included lighting and texturing, which I solved based on previous experience.
              </p>
              <p>
                <em>Disclaimer</em>: Demo videos may appear laggy; higher-quality videos are available in the{" "}
                <a
                  className="highlight-link"
                  href="https://github.com/Danielhoyland/Portfolio/tree/main/Grafikk%20-%20C%2B%2B%20OpenGL/assignment"
                  target="_blank"
                  rel="noopener noreferrer"
                >
                  project code
                </a>
                .
              </p>
            </div>
          </div>
        </section>
      </div>
    </>
  );
}
