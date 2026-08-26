import React from "react";
import "../../css/pages.css";

export default function MachineLearningDetail() {
  return (
    <>
      <div className="sectionDiv">
        {/* Description Section */}
        <section className="description-section">
          <h2 className="section-title">Course Overview</h2>
          <p>
            This course introduced the fundamental concepts behind data analysis
            and machine learning. The focus was on understanding how models can
            learn patterns from data, how different learning approaches are
            applied, and how to evaluate whether a model is suitable for a given
            problem.
          </p>
          <p>
            Throughout the course, I worked with multiple machine learning tasks
            involving supervised learning, unsupervised learning,
            multi-class classification, and reinforcement learning. The
            assignments combined theoretical understanding with practical
            implementation using Python and common machine learning libraries.
          </p>
        </section>

        {/* Learning and Technology Section */}
        <section className="learning-tech-section">
          <div className="what-i-learned">
            <h2 className="section-title">Key Takeaways</h2>
            <p>
              Through this course I gained practical experience with the full
              machine learning workflow, including data preparation, feature
              analysis, model training, evaluation, and visualization. I also
              developed a better understanding of the mathematical principles
              behind machine learning algorithms.
            </p>

            <ul className="custom-list">
              <li>
                Applied supervised and unsupervised learning techniques
              </li>
              <li>
                Used dimensionality reduction methods to analyze complex
                datasets
              </li>
              <li>
                Trained and evaluated machine learning models using Python
                libraries
              </li>
            </ul>
          </div>

          <div className="tech-used">
            <h3 className="subsection-title">Technologies & Tools</h3>
            <ul className="custom-list">
              <li>Scikit-learn</li>
              <li>TensorFlow / Keras</li>
              <li>Jupyter Notebook</li>
              <li>Data Visualization</li>
            </ul>

            <h3 className="subsection-title">Machine Learning Methods</h3>
            <ul className="custom-list">
              <li>PCA</li>
              <li>t-SNE</li>
              <li>UMAP</li>
              <li>KMeans Clustering</li>
              <li>Neural Networks</li>
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
              <h3 className="section-title">Course Details</h3>

              <p>
                The course consisted of multiple assignments where different
                machine learning approaches were applied to analyze datasets
                and solve prediction problems. The focus was not only on
                achieving good results, but also understanding why a model
                performed well and when it was appropriate to use.
              </p>

              <p>
                In supervised learning tasks, I worked with labeled datasets
                where models learned patterns from existing examples. This
                included classification problems such as the Iris dataset,
                where different algorithms were trained and evaluated based on
                their ability to correctly classify data samples.
              </p>

              <p>
                I also explored unsupervised learning methods where patterns
                needed to be discovered without predefined labels. Using
                techniques such as KMeans clustering, PCA, t-SNE, and UMAP, I
                analyzed high-dimensional datasets and visualized hidden
                structures within the data.
              </p>

              <p>
                Another important part of the course was understanding neural
                networks. Using TensorFlow and Keras, I gained practical
                experience creating and training models while learning about
                the challenges of model selection, overfitting, and evaluation.
              </p>

              <p>
                The course also introduced reinforcement learning and
                explainability methods, providing insight into how models can
                learn from interaction and how decisions made by machine
                learning systems can be interpreted.
              </p>

              <p>
                Overall, this course gave me a solid foundation in machine
                learning by combining theoretical principles with practical
                implementation. I gained experience not only building models,
                but also understanding their limitations and choosing suitable
                methods based on the data and problem requirements.
              </p>

              <p>
                <a
                  className="highlight-link"
                  href="https://github.com/Danielhoyland/Portfolio/tree/main/projects/Introduction%20To%20Machine%20Learning"
                  target="_blank"
                  rel="noopener noreferrer"
                >
                  View course project on GitHub
                </a>
              </p>
            </div>
          </div>
        </section>
      </div>
    </>
  );
}