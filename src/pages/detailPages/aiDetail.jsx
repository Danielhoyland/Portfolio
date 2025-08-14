import React from "react";
import "../../../style.css";

export default function AlbumDetails() {
  return (
    <>
      <div className="sectionDiv">

        {/* Description Section */}
        <section className="description-section">
          <h2 className="section-title">Course Overview</h2>
          <p>
            This course explores <strong>data-driven AI</strong> with a focus on 
            machine learning, Bayesian networks, deep learning, and their 
            real-world applications in <em>image processing</em> and 
            <em> natural language processing</em>.  
            Practical examples and projects are woven into the curriculum, giving
            students the opportunity to follow an AI project from concept to completion.
          </p>
          <p>
            Learning is reinforced through <strong>hands-on labs</strong> and 
            <strong> milestone-based assignments</strong> designed to build both 
            technical skills and problem-solving ability.
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
              I gained a solid understanding of the <strong>mathematical foundations</strong> 
              behind AI and how algorithms learn to make predictions.  
              I developed models capable of:
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
              <h3 className="section-title">Object Detection — Cars, Cats, and Dogs</h3>
              <p>
                One project involved training an AI to detect and classify objects
                in images.  
                In one case, we taught it to <strong>distinguish between cats and dogs</strong>.  
                In another, we trained a model to detect cars and output 
                <em> segmentation masks</em> highlighting them in the image.
              </p>
            </div>
          </div>

          {/* Example 2 */}
          <div className="row">
            <div className="text">
              <h3 className="section-title">Emotion Recognition</h3>
              <p>
                I also developed an AI model for <strong>emotion detection</strong> 
                using facial images.  
                The model learned to identify emotions such as happiness, anger, and surprise.
              </p>
              <p>
                This task was more resource-intensive — each emotion required 
                a large training dataset. Due to RAM limitations, I had to 
                reduce both the number of training images and the training epochs, 
                resulting in a <strong>53% accuracy</strong> score.
              </p>
              <p>
                <a 
                  className="highlight-link"
                  href="https://github.com/Danielhoyland/Portfolio/tree/main/Kunstliginteligens"
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
