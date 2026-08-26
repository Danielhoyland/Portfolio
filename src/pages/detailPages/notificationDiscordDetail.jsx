import React from "react";
import "../../css/pages.css";

export default function NotificationDiscordDetail() {
  return (
    <>
      <div className="sectionDiv">
        {/* Description Section */}
        <section className="description-section">
          <h2 className="section-title">Project Overview</h2>
          <p>
            This personal project was created to automatically monitor a website
            for changes and notify a user when new information was published.
            The goal was to avoid manually checking a website repeatedly by
            creating an automated notification system using GitHub Actions and
            Discord webhooks.
          </p>
          <p>
            The system periodically fetched the website content, generated a
            hash representation of the page, and compared it against the
            previously stored hash. If a change was detected, a notification
            was automatically sent through Discord.
          </p>
        </section>

        {/* Learning and Technology Section */}
        <section className="learning-tech-section">
          <div className="what-i-learned">
            <h2 className="section-title">Key Takeaways</h2>
            <p>
              This project gave me practical experience with automation,
              continuous integration workflows, and designing reliable scheduled
              tasks. It also taught me about the limitations of free cloud
              infrastructure and how to work around unreliable scheduling
              systems.
            </p>

            <ul className="custom-list">
              <li>
                Built an automated website monitoring and notification system
              </li>
              <li>
                Worked with GitHub Actions workflows and scheduled execution
              </li>
              <li>
                Implemented content hashing to detect website changes
              </li>
            </ul>
          </div>

          <div className="tech-used">
            <h3 className="subsection-title">Technologies & Tools</h3>
            <ul className="custom-list">
              <li>GitHub Actions</li>
              <li>Discord Webhooks</li>
              <li>GitHub Secrets</li>
              <li>Cron Scheduling</li>
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
              <h3 className="section-title">Project Details</h3>

              <p>
                The project started from a simple problem: a website contained
                important updates, but manually checking it frequently was
                inefficient. I created a small monitoring system that could
                automatically check the website and notify my partner whenever
                new information was added.
              </p>

              <p>
                The implementation was written in Python and executed through a
                scheduled GitHub Actions workflow. Each execution downloaded
                the website content, calculated a hash value, and compared it
                with the previously stored version. If the hash changed, the
                workflow triggered a Discord webhook notification containing
                information that the page had been updated.
              </p>

              <p>
                One of the main challenges was scheduling reliability. GitHub
                Actions' free scheduled runners do not guarantee exact
                execution times, as workflows can be delayed when placed in a
                queue. Since the goal was to check updates approximately every
                ten minutes, this created reliability issues.
              </p>

              <p>
                To solve this, I introduced an additional external cron
                scheduler that triggered the GitHub Actions workflow through the
                GitHub API. This created a more reliable scheduling mechanism
                while still keeping the project completely free to run.
              </p>

              <p>
                Through this project I gained experience with practical
                automation challenges, including state persistence, API
                integration, secrets management, scheduled jobs, and designing
                solutions around the limitations of free cloud services.
              </p>

              <p>
                <a
                  className="highlight-link"
                  href="https://github.com/Danielhoyland/Portfolio/tree/main/projects/DiscordBot"
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