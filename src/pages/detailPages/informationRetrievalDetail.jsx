import React from "react";
import "../../css/pages.css";

export default function InformationRetrievalDetail() {
  return (
    <>
      <div className="sectionDiv">
        {/* Description Section */}
        <section className="description-section">
          <h2 className="section-title">Course Overview</h2>
          <p>
            This course introduced the fundamental concepts behind modern search
            engines and information retrieval systems. The focus was on how
            large collections of documents can be stored, indexed, searched,
            and ranked to provide users with the most relevant results.
          </p>
          <p>
            Throughout the semester, I worked on different information retrieval
            tasks involving document processing, indexing, query handling, and
            ranking algorithms. The course also focused on understanding the
            mathematical foundations behind retrieval models and why certain
            approaches produce better search results than others.
          </p>
        </section>

        {/* Learning and Technology Section */}
        <section className="learning-tech-section">
          <div className="what-i-learned">
            <h2 className="section-title">Key Takeaways</h2>
            <p>
              Through this course I gained a deeper understanding of how search
              engines work internally, from processing raw documents to ranking
              relevant results. I learned how mathematical models can be used
              to measure similarity between documents and user queries.
            </p>

            <ul className="custom-list">
              <li>
                Implemented and evaluated information retrieval algorithms
              </li>
              <li>
                Learned about indexing, document processing, and query
                optimization
              </li>
              <li>
                Applied mathematical models for ranking and relevance scoring
              </li>
            </ul>
          </div>

          <div className="tech-used">
            <h3 className="subsection-title">Technologies & Tools</h3>
            <ul className="custom-list">
              <li>Jupyter Notebook</li>
              <li>Data Processing Tools</li>
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
                The assignments throughout the semester focused on different
                parts of an information retrieval pipeline. This included
                transforming documents into searchable representations,
                creating indexes, processing user queries, and determining
                which documents were most relevant.
              </p>

              <p>
                A major part of the course was understanding the mathematical
                reasoning behind retrieval systems. I worked with concepts such
                as term weighting, similarity calculations, ranking functions,
                and evaluation methods to understand how search engines decide
                which results should appear first.
              </p>

              <p>
                The course also introduced modern approaches to information
                retrieval, including web search techniques, neural search, and
                multimedia retrieval. This provided insight into how traditional
                search algorithms have evolved into the large-scale systems used
                by modern search engines.
              </p>

              <p>
                Overall, this course gave me a better understanding of the
                complexity behind seemingly simple search functionality. It
                demonstrated how data structures, mathematics, and algorithms
                work together to create efficient systems for finding relevant
                information from massive datasets.
              </p>

              <p>
                <a
                  className="highlight-link"
                  href="https://github.com/Danielhoyland/Portfolio/tree/main/projects/Information%20Retrival"
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