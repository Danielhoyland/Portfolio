import React from 'react';
import '../style.css'; 
import GHlogo from './svg/GitHub_logo.svg';

const Footer = () => {
  return (
    <footer className="site-footer">
      <div className="footer-content">
        <div className="footer-section">
          <h3>Daniel Høyland</h3>
          <a href="mailto:Danielhoyland@hotmail.com">Danielhoyland@hotmail.com</a>
          <p>+472662229</p>
        </div>

        <div className="footer-section">
          <div style={{ display: 'flex', alignItems: 'center', gap: '8px' }}>
            <img
              id="github-icon"
              alt="GitHub Icon"
              src={GHlogo}
              style={{ width: '24px', height: '24px' }}
            />
            <a href="https://github.com/Danielhoyland" target="_blank" rel="noopener noreferrer">
              Daniehoy
            </a>
          </div>
        </div>

        <div className="footer-section">
          <p>
            My portfolio, preview some of my skills and previous projects I have worked with!<br />
            Feel free to send me an E-mail!
          </p>
        </div>
      </div>
    </footer>
  );
};
export default Footer;