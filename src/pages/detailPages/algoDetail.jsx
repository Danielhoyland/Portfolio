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
            This course introduced me to a wide spectrum of programming
            languages and paradigms, with a focus on multi-paradigm approaches
            and modern language features. The topics covered included lambda
            calculus, functional programming, lazy computations, memory
            management, event-driven and reactive programming, multi-threading,
            concurrent programming patterns, and even novel techniques such as
            Smart Contracts.
          </p>
          <p>
            Languages explored included Rust, Haskell, Golang, Java/Kotlin,
            Dart, and Solidity, with references to modern C++ and other
            languages.
          </p>
          <p>
            <a
              className="highlight-link"
              href="https://www.ntnu.edu/studies/courses/PROG2006#tab=omEmnet"
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
              Through this course I gained hands-on experience with both
              <strong> Haskell</strong> and <strong>Rust</strong>. I improved my
              overall programming practices, developed a deeper understanding of
              programming paradigms, and learned how to apply best practices in
              challenging environments.
            </p>
            <ul className="custom-list">
              <li>Applied recursion in functional programming</li>
              <li>Explored concurrency and memory management patterns</li>
              <li>Worked with interpreters and custom game logic</li>
            </ul>
          </div>

          <div className="tech-used">
            <h3 className="subsection-title">Technologies & Tools</h3>
            <ul className="custom-list">
              <li>SDL2</li>
            </ul>
            <h3 className="subsection-title">Programming Languages</h3>
            <ul className="custom-list">
              <li>Rust</li>
              <li>Haskell</li>
              <li>Golang</li>
            </ul>
          </div>
        </section>

        {/* Examples Section */}
        <section className="alternating-sections">
          {/* Example 1 */}
          <div className="row">
            <div className="text">
              <h3 className="section-title">Haskell Project</h3>
              <p>
                One of the main assignments was to recreate the board game{" "}
                <em>GO</em>. Since Haskell doesn’t support traditional loops, I
                had to rely on recursive functions. At first this was very
                difficult to grasp, and I even ended up creating an extremely
                compact function that I would now avoid at all costs, haha.
              </p>
              <p>
                You can test the game itself on{" "}
                <a
                  className="highlight-link"
                  href="https://online-go.com/"
                  target="_blank"
                  rel="noopener noreferrer"
                >
                  this site
                </a>
                , or view my implementation on GitHub{" "}
                <a
                  className="highlight-link"
                  href="https://github.com/Danielhoyland/Portfolio/blob/main/Avansert%20-%20Rust%20Haskell/Assignments/Assignment1/src/Lib.hs"
                  target="_blank"
                  rel="noopener noreferrer"
                >
                  here
                </a>
                .
              </p>
            </div>
            <div className="image">
              <img src="images/GO.png" alt="Go game" />
            </div>
          </div>

          {/* Example 2 */}
          <div className="row">
            <div className="image">
              <img src="images/Interperter.png" alt="Interpreter" />
            </div>
            <div className="text">
              <h3 className="section-title">Rust Project</h3>
              <p>
                Another assignment was to build a custom interpreter. It needed
                to handle mathematical operations as well as functions like{" "}
                <code>dup</code> (duplicate input). I found the project really
                enjoyable, but due to time constraints I only managed to finish
                about 50% before the deadline.
              </p>
              <p>
                Despite that, it was a great introduction to Rust’s powerful
                features, and it made me appreciate how fun programming in Rust
                can be.
              </p>
              <p>
                <a
                  className="highlight-link"
                  href="https://github.com/Danielhoyland/Portfolio/tree/main/Avansert%20-%20Rust%20Haskell"
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
