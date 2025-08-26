import React, { useState, useEffect } from "react";
import "./Details.css";
import ImageCarousel from "./ImageCarousel";

export default function AlbumDetailsTemplate({
  header,
  content,
  images = [],
  icons = [],
  link = null, // { href: '', img: '', label: '' }
}) {
  const [currentIndex, setCurrentIndex] = useState(0);
  const [showCounter, setShowCounter] = useState(true);


  const handlePrev = () => {
    setCurrentIndex((prev) => (prev === 0 ? images.length - 1 : prev - 1));
    setShowCounter(true);
  };

  const handleNext = () => {
    setCurrentIndex((prev) => (prev === images.length - 1 ? 0 : prev + 1));
    setShowCounter(true);
  };

  return (
    <div className="album-details-container">
      <header className="album-header">
        <h1>{header}</h1>
      </header>

      <div className="album-content-wrapper">
        <div
          className="album-content-scrollable"
          style={{

          }}
        >
          {content}
        </div>

        <div
          className="album-image-viewer"
        >
          {images.length > 0 && <ImageCarousel images={images} />}
        </div>
      </div>

      <footer className="album-footer">
        <div className="footer-icons">
          {icons.map((icon, i) => (
            <span key={i}>{icon}</span>
          ))}
        </div>
        {link && (
          <div className="footer-link">
            <a href={link.href} target="_blank" rel="noopener noreferrer">
              {link.img && (
                <img
                  src={link.img}
                  alt={link.label || "link"}
                  className="link-icon"
                />
              )}
              {link.label && <span>{link.label}</span>}
            </a>
          </div>
        )}
      </footer>
    </div>
  );
}
