import React from "react";
import "./css/footer.css";
import { GitHubIcon, LinkedInIcon } from "./components/IconComponent";

const Footer = () => {
  return (
    <footer className="site-footer">
      <div className="footer-content">
        <div className="footer-section">
          <h3>Daniel Høyland</h3>
          <a href="mailto:Danielhoyland@hotmail.com">
            Danielhoyland@hotmail.com
          </a>
          <p>+472662229</p>
        </div>

        <div className="footer-section">
          {GitHubIcon}
          <a
            href="https://github.com/Danielhoyland"
            target="_blank"
            rel="noopener noreferrer"
          >
            DanielHoyland
          </a>

          <div style={{ height: "10px" }}></div>

          {LinkedInIcon}
          <a
            href="www.linkedin.com/in/daniel-høyland-21468a17a"
            target="_blank"
            rel="noopener noreferrer"
          >
            Daniel Høyland
          </a>
        </div>

        <div className="footer-section">
          <p>
            My portfolio, preview some of my skills and previous projects I have
            worked with!
            <br />
            Feel free to send me an E-mail!
          </p>
        </div>
      </div>
    </footer>
  );
};
export default Footer;
