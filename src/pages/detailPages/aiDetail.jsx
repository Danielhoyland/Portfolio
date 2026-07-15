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
            In this course I explored data-driven AI where the focus was on
            machine learning, Bayesian networks, deep learning, and their
            real-world applications in image processing and natural language
            processing.
          </p>
          <p>
            I got to learning AI through hands-on labs and assignments that were
            designed to teach me and other fellow students machine learning and
            core concepts in AI and machine learning.
          </p>
          <p>
            <a
              className="highlight-link"
              href="https://www.ntnu.edu/studies/courses/PROG2051/#tab=omEmnet"
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
              I gained a solid fundamental understanding of the mathematical
              behind low level AI and how algorithms learn to make predictions.
              I also got to make models capable of:
            </p>
            <ul className="custom-list">
              <li>Detecting and classifying language</li>
              <li>Categorizing images, text, and video</li>
              <li>Performing object detection and segmentation</li>
            </ul>
          </div>

          <div className="tech-used">
            <h3 className="subsection-title">Technologies & Tools</h3>
            <ul className="custom-list">
              <li>Jupyter Notebook</li>
              <li>Google Colab</li>
              <li>PyTorch</li>
            </ul>
            <h3 className="subsection-title">Programming Languages</h3>
            <ul className="custom-list">
              <li>Python</li>
            </ul>
          </div>
        </section>

        {/* Examples Section */}
        <section className="alternating-sections">
          {/* Example 1 */}
          <div className="row">
            <div className="text">
              <h3 className="section-title">
                Object Detection — Cars, Cats, and Dogs
              </h3>
              <p>
                One assignment involved training an AI to detect and classify
                objects in images. In one case, we taught it to distinguish
                between cats and dogs and then give an answer in text if it was
                a cat or a dog. In another, we trained a model to detect cars
                and output segmentation masks highlighting them in the image.
              </p>
            </div>
          </div>

          {/* Example 2 */}
          <div className="row">
            <div className="text">
              <h3 className="section-title">Emotion Recognition</h3>
              <p>
                In another task we made a emotion detection ai through public
                datasets using facial images. The model learned to identify
                emotions such as happiness, anger, and surprise, with varying
                degree of accuracy.
              </p>
              <p>
                This task required a lot of ram to let the AI to have a big
                enough batch size to actually find the underlying pattern. Each
                emotion required a large labeled training dataset. Due to RAM
                limitations, I had to reduce both the number of training images
                and the training epochs, resulting in a 53% accuracy score.
              </p>
              <p>
                <a
                  className="highlight-link"
                  href="https://github.com/Danielhoyland/Portfolio/tree/main/projects/Kunstliginteligens"
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
