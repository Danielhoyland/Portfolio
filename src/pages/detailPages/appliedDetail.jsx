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
            This course focused on applied data science, emphasizing practical
            use of data-driven methods to solve real-world problems. Instead of
            only exploring advanced algorithms, the course highlighted how to
            design complete data science solutions, from defining business
            problems to deploying and monitoring models.
          </p>
          <p>
            A key part of the course was understanding the full lifecycle of a
            data science project, including problem definition, data
            preprocessing, model development, and evaluation in a real-world
            context.
          </p>
          <p>
            <a
              className="highlight-link"
              href="https://www.ntnu.edu/studies/courses/TDT4259#tab=omEmnet"
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
              I gained hands-on experience with the full data science workflow,
              focusing on solving practical and business-relevant problems. This
              included:
            </p>
            <ul className="custom-list">
              <li>Defining data-driven problems and business objectives</li>
              <li>Preprocessing and analyzing real-world datasets</li>
              <li>Building and evaluating machine learning models</li>
              <li>Interpreting results and identifying key influencing factors</li>
              <li>Designing solutions for real-world deployment</li>
            </ul>
          </div>

          <div className="tech-used">
            <h3 className="subsection-title">Technologies & Tools</h3>
            <ul className="custom-list">
              <li>Jupyter Notebook</li>
              <li>Pandas & NumPy</li>
              <li>Scikit-learn</li>
              <li>Matplotlib / Seaborn</li>
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
                Student Dropout Prediction System
              </h3>
              <p>
                The main project focused on predicting student dropout in higher
                education. The goal was to help institutions identify students
                at risk early and provide targeted support.
              </p>
              <p>
                We analyzed demographic, academic, and socio-economic data to
                uncover patterns contributing to dropout. Based on this, we built
                machine learning models that estimate the probability of a
                student dropping out.
              </p>
            </div>
          </div>

          {/* Example 2 */}
          <div className="row">
            <div className="text">
              <h3 className="section-title">
                Data Analysis, Modeling & Deployment
              </h3>
              <p>
                The project followed a structured data science workflow inspired by
                CRISP-DM, covering data understanding, preprocessing, modeling, and
                evaluation.
              </p>
              <p>
                We handled real-world challenges such as class imbalance, feature
                selection, and correlation analysis to identify the most influential
                factors — with academic performance emerging as the strongest predictor
                of student outcomes.
              </p>
              <p>
                Beyond modeling, we designed a deployable solution including a REST API
                and a web-based dashboard. The system allows institutions to upload
                student data, receive dropout probability predictions, and identify key
                risk factors for each student.
              </p>
              <p>
                The solution also included monitoring concepts such as performance KPIs
                (precision, recall, F1-score), alert systems for high-risk students, and
                strategies for continuous model maintenance and retraining.
              </p>
              <p>
                <a
                  className="highlight-link"
                  href="https://github.com/Danielhoyland/Portfolio/tree/main/Applied%20datascience%20-%20Python"
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