// ProjectCard.jsx
import React from "react";
import Slider from "react-slick";
import "./ProjectCard.css"; // You can copy your earlier CSS or modify it

const ProjectCard = ({ title, description, tools, images }) => {
  const sliderSettings = {
    dots: true,
    arrows: false,
    infinite: true,
    vertical: true,
    verticalSwiping: true,
    speed: 500,
    slidesToShow: 1,
    slidesToScroll: 1,
    autoplay: true,
    autoplaySpeed: 3000,
  };

  return (
    <div className="project-card w-full sm:w-1/2 md:w-1/3 lg:w-1/4">

      <div className="tools-bar">
        {tools.map((icon, idx) => (
  <img
  key={idx}
  src={icon}
  alt={`Tool ${idx}`}
  className="tool-icon w-6 h-6 mx-1"
/>

))}

      </div>

      <Slider {...sliderSettings} className="project-slider">
        {images.map((img, idx) => (
          <div key={idx}>
            <img src={img} alt={`Slide ${idx}`} className="project-image" />
          </div>
        ))}
      </Slider>

      <div className="project-content">
        <h3>{title}</h3>
        <p className="project-description">{description}</p>
      </div>
    </div>
  );
};

export default ProjectCard;
