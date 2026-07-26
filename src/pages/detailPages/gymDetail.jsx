import React from "react";
import "../../css/pages.css";

export default function GymDetail() {
  return (
    <>
      <div className="sectionDiv">
        {/* Description Section */}
        <section className="description-section">
          <h2 className="section-title">Project Overview</h2>
          <p>
            This is a personal Android application that I am developing to make
            gym tracking as simple and efficient as possible. The goal is to
            automatically recommend workouts based on previous training history,
            muscle recovery, and user preferences while providing an intuitive
            interface for logging exercises during a workout.
          </p>
          <p>
            The project has a strong focus on software architecture and user
            experience. Before beginning implementation, I designed the complete
            database structure, application flow, and user interface to ensure
            the application remains easy to use while supporting advanced
            workout recommendations.
          </p>
        </section>

        {/* Learning and Technology Section */}
        <section className="learning-tech-section">
          <div className="what-i-learned">
            <h2 className="section-title">Key Takeaways</h2>
            <p>
              Although the implementation is still ongoing, the planning phase
              has allowed me to practice designing a scalable mobile
              application, creating a normalized database, and thinking through
              user interactions before writing code.
            </p>
            <ul className="custom-list">
              <li>Designed a normalized SQLite database from scratch</li>
              <li>Created complete UI/UX flows before implementation</li>
              <li>
                Planned a recommendation system based on workout history and
                muscle recovery
              </li>
            </ul>
          </div>

          <div className="tech-used">
            <h3 className="subsection-title">Technologies & Tools</h3>
            <ul className="custom-list">
              <li>Android Studio</li>
              <li>SQLite</li>
              <li>Miro</li>
              <li>Git</li>
            </ul>

            <h3 className="subsection-title">Programming Languages</h3>
            <ul className="custom-list">
              <li>Kotlin</li>
            </ul>
          </div>
        </section>

        {/* Examples Section */}
        <section className="alternating-sections">
          <div className="row">
            <div className="text">
              <h3 className="section-title">Project Details</h3>

              <p>
                Rather than jumping directly into development, I started by
                designing the application's architecture. I created a complete
                database schema covering exercises, muscle groups, workout
                history, user settings, body weight tracking, exercise
                animations, and recommendation settings. This allows the
                application to support advanced features while keeping the data
                organized and scalable.
              </p>

              <p>
                The user interface was designed in Miro with a strong focus on
                minimizing the number of interactions required during a workout.
                Once a workout begins, the user stays within the workout flow
                until it is completed, making it quick to log sets, repetitions,
                and weights without unnecessary navigation.
              </p>

              <p>
                One of the main planned features is an intelligent workout
                recommendation system. Based on previous workouts, selected
                training style (such as Full Body or Push/Pull/Legs), recovery
                time for muscle groups, and user preferences, the application
                will automatically suggest appropriate exercises and recommend
                working weights using previous performance and progressive
                overload principles.
              </p>

              <p>
                Although the project is still in development, considerable time
                has already been invested in the software design, database
                architecture, and user experience to build a solid foundation
                before implementation begins.
              </p>

              <p>
                <a
                  className="highlight-link"
                  href="#"
                  target="_blank"
                  rel="noopener noreferrer"
                >
                  GitHub repository coming soon
                </a>
              </p>
            </div>
          </div>
        </section>
      </div>
    </>
  );
}