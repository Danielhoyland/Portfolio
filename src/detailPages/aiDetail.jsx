import React from "react";
import "../../style.css"; 

export default function AlbumDetails() {
  return (
    <>
      {/* Header component */}
      <div id="header"></div>

      <div className="sectionDiv">
        {/* Main Header */}
        <header className="main-header">
          <h1>Artificial Intelligence</h1>
          <h2>PROG2051</h2>
        </header>

        {/* Description Section */}
        <section className="description-section">
          <h2>Description</h2>
          <p>
            The course focuses on data-driven AI topics including machine
            learning, Bayesian networks, deep learning, and popular applications
            in image processing and natural language processing. Practical
            applications and real-world examples are carefully walked through so
            that students can follow and understand a complete AI project. Lab
            exercises and obligatory assignments are important instruments to
            ensure learning progress with well-defined milestones.
          </p>
          <p>
            Read more at the{" "}
            <a href="https://www.ntnu.edu/studies/courses/PROG2051/#tab=omEmnet">
              subject site
            </a>
            .
          </p>
        </section>

        {/* Learning and Technology Section */}
        <section className="learning-tech-section">
          <div className="what-i-learned">
            <h2>What I Have Learned</h2>
            <p>
              In this subject, I gained a fundamental understanding of the
              mathematical foundations behind AI and how it creates
              connections. I also learned how to develop simple AI models
              capable of detecting language and categorizing text, images,
              videos, and performing more detailed detection tasks.
            </p>
          </div>

          <div className="tech-used">
            <h3>Technologies Used</h3>
            <ul>
              <li>Jupyter Notebook</li>
              <li>Google Colab</li>
              <li>PyTorch</li>
            </ul>
            <h3>Programming Languages Used</h3>
            <ul>
              <li>Python</li>
            </ul>
          </div>
        </section>

        {/* Alternating Text and Image Sections */}
        <section className="alternating-sections">
          {/* Text Left, Image Right */}
          <div className="row">
            <div className="text">
              <h3>Example of an AI I created during the course</h3>
              <p>
                As shown in the image to the right, we applied image detection
                AI during the course. For instance, we used it to differentiate
                between cats and dogs, and to detect cars in images, as
                demonstrated by the mask on the right. In this task, we trained
                an AI model on car images, enabling it to generate a mask that
                highlights all objects identified as cars.
              </p>
            </div>
            <div className="image">
              <img src="images/CarMask.jpg" alt="AI car mask" />
            </div>
          </div>

          {/* Image Left, Text Right */}
          <div className="row">
            <div className="image">
              <img
                src="images/suprise.png"
                alt="Emotion detection"
                style={{ width: "49.5%" }}
              />
              <img
                src="images/fearSad.png"
                alt="Emotion detection"
                style={{ width: "49.5%" }}
              />
            </div>
            <div className="text">
              <h3>Example of more complex AI detection</h3>
              <p>
                In a similar manner to the previous example, I developed an
                emotion detection AI. It was trained using facial images
                displaying various emotions, allowing the model to learn how to
                identify emotions in human faces. However, this task required
                significantly more RAM than earlier projects, as each emotion
                needed a large set of images for the AI to learn effectively.
                Unfortunately, due to insufficient RAM, I had to reduce the
                number of training images and limit the number of epochs. As a
                result, the AI achieved only 53% accuracy.
              </p>
              <p>
                If you are interested in the code of the tasks I did in this
                subject, click{" "}
                <a href="https://github.com/Danielhoyland/Portfolio/tree/main/Kunstliginteligens">
                  here
                </a>
                .
              </p>
            </div>
          </div>
        </section>
      </div>

      {/* Footer component */}
      <div id="footer"></div>


    </>
  );
}
