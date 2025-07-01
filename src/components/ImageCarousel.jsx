import React from "react";
import { Carousel } from "react-responsive-carousel";
import "react-responsive-carousel/lib/styles/carousel.min.css";
import './ImageCarousel.css'; 

export default function ImageCarousel({ images = [] }) {
  return (
    <Carousel
      showThumbs={false}
      infiniteLoop={true}
      useKeyboardArrows={true}
      autoPlay
      emulateTouch={true}
      showStatus={false}
      dynamicHeight={false} 
      renderArrowPrev={(onClickHandler, hasPrev, label) =>
        hasPrev && (
          <button
            type="button"
            onClick={onClickHandler}
            title={label}
            className="custom-arrow custom-prev"
          >
            ‹
          </button>
        )
      }
      renderArrowNext={(onClickHandler, hasNext, label) =>
        hasNext && (
          <button
            type="button"
            onClick={onClickHandler}
            title={label}
            className="custom-arrow custom-next"
          >
            ›
          </button>
        )
      }
    >
      {images.map((img, idx) => (
        <div key={idx}>
          <img
            src={img.src || img}
            alt={img.alt || `Slide ${idx + 1}`}
            style={{
              maxHeight: "70vh",
              objectFit: "contain", // or "cover" depending on your goal
              width: "100%",
            }}
          />
          <div className="custom-caption-overlay">
            <span className="image-counter">{`${idx + 1} / ${
              images.length
            }`}</span>
            {img.caption && <p className="image-caption">{img.caption}</p>}
          </div>
        </div>
      ))}
    </Carousel>
  );
}
