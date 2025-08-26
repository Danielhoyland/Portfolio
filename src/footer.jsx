import React from "react";
import "./css/footer.css";
import { GitHubIcon, LinkedInIcon } from "./components/IconComponent";

const Footer = () => {
  return (
    <footer className="site-footer">
      <div className="footer-content">
        {/* Identity */}
        <div className="footer-section identity">
          <h3>Daniel Høyland</h3>
          <a href="mailto:Danielhoyland@hotmail.com">
            Danielhoyland@hotmail.com
          </a>
          <p>+47 266 2229</p>
        </div>

        {/* Links */}
        <div className="footer-section links">
          <h4>Connect</h4>
          <a
            href="https://github.com/Danielhoyland"
            target="_blank"
            rel="noopener noreferrer"
          >
            {GitHubIcon} GitHub
          </a>
          <a
            href="https://www.linkedin.com/in/daniel-høyland-21468a17a"
            target="_blank"
            rel="noopener noreferrer"
          >
            {LinkedInIcon} LinkedIn
          </a>
        </div>


        <div className="footer-section cta">
          <p>
            Explore my portfolio to preview some of my projects and skills.
            <br />
            <strong>Let's connect and create something together!</strong>
          </p>
        </div>
      </div>

      {/* Copyright bar */}
      <div className="footer-bottom">
        <p>© {new Date().getFullYear()} Daniel Høyland. All rights reserved.</p>
      </div>
    </footer>
  );
};
export default Footer;
