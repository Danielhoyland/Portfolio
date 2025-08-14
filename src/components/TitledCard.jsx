import { useRef, useState, useEffect } from "react";
import {
  motion,
  useMotionValue,
  useSpring,
} from "framer-motion";
import Modal from "./Modal"; 
import "./TitledCard.css"; 
import AlbumDetails from "./Details.jsx";

const springValues = {
  damping: 30,
  stiffness: 100,
  mass: 2,
};

export default function TiltedCard({
  altText = "Tilted card image",
  captionText = "",
  containerHeight = "400px",
  containerWidth = "400px",
  imageHeight = "400px",
  imageWidth = "400px",
  scaleOnHover = 1.1,
  rotateAmplitude = 14,
  showMobileWarning = true,
  modalContent = null,
  overlayContent = null,
  displayOverlayContent = false,
  svgIcons = [],
  hoverText = "",
  imageList = [],
}) {
  const ref = useRef(null);
  const [lastY, setLastY] = useState(0);
  const [isModalOpen, setIsModalOpen] = useState(false);
  const [currentIndex, setCurrentIndex] = useState(0);
  const [prevIndex, setPrevIndex] = useState(null);
  const [direction, setDirection] = useState(1); 

  const images = imageList.length > 0 ? imageList : [];

  const x = useMotionValue();
  const y = useMotionValue();
  const rotateX = useSpring(useMotionValue(0), springValues);
  const rotateY = useSpring(useMotionValue(0), springValues);
  const scale = useSpring(1, springValues);
  const opacity = useSpring(0);
  const rotateFigcaption = useSpring(0, {
    stiffness: 350,
    damping: 30,
    mass: 1,
  });

  useEffect(() => {
    const interval = setInterval(() => {
      setDirection(1); 
      setPrevIndex(currentIndex);
      setCurrentIndex((prev) => (prev + 1) % images.length);
    }, 4000);

    return () => clearInterval(interval);
  }, [currentIndex, images.length]);
  function handleMouse(e) {
    if (!ref.current) return;

    const rect = ref.current.getBoundingClientRect();
    const offsetX = e.clientX - rect.left - rect.width / 2;
    const offsetY = e.clientY - rect.top - rect.height / 2;

    const rotationX = (offsetY / (rect.height / 2)) * -rotateAmplitude;
    const rotationY = (offsetX / (rect.width / 2)) * rotateAmplitude;

    rotateX.set(rotationX);
    rotateY.set(rotationY);

    x.set(e.clientX - rect.left);
    y.set(e.clientY - rect.top);

    const velocityY = offsetY - lastY;
    rotateFigcaption.set(-velocityY * 0.6);
    setLastY(offsetY);
  }

  function handleMouseEnter() {
    scale.set(scaleOnHover);
    opacity.set(1);
  }

  function handleMouseLeave() {
    opacity.set(0);
    scale.set(1);
    rotateX.set(0);
    rotateY.set(0);
    rotateFigcaption.set(0);
  }

  function openModal() {
    setIsModalOpen(true);
  }

  function closeModal() {
    setIsModalOpen(false);
  }

  return (
    <>
      <figure
        ref={ref}
        className="tilted-card-figure"
        style={{ height: containerHeight, width: containerWidth }}
        onMouseMove={handleMouse}
        onMouseEnter={handleMouseEnter}
        onMouseLeave={handleMouseLeave}
        onClick={openModal}
      >
        {showMobileWarning && (
          <div className="tilted-card-mobile-alert">
            This effect is not optimized for mobile. Check on desktop.
          </div>
        )}

        {svgIcons.length > 0 && (
          <motion.div
            className="svg-icon-container"
            style={{ rotateX, rotateY, opacity, scale }}
          >
            {svgIcons.map((icon, i) => (
              <span className="svg-icon" key={i}>
                {icon}
              </span>
            ))}
          </motion.div>
        )}
        <motion.div
          className="dark-hover-overlay"
          style={{ rotateX, rotateY, opacity, scale }}
        />
        <motion.div
          className="card-title-overlay"
          style={{ rotateX, rotateY, scale }}
        >
          {captionText}
        </motion.div>

        {/* Hover Text */}
        {hoverText && (
          <motion.div
            className="tilted-card-hover-text"
            style={{ rotateX, rotateY, opacity, scale }}
          >
            {hoverText}
          </motion.div>
        )}
        <motion.div
          className="tilted-card-img"
          style={{
            width: imageWidth,
            height: imageHeight,
            rotateX,
            rotateY,
            scale,
            position: "relative",
            overflow: "hidden",
          }}
        >
          <div
            className="tilted-card-img"
            style={{
              width: imageWidth,
              height: imageHeight,
              position: "relative",
              overflow: "hidden",
            }}
          >
            {prevIndex !== null && (
              <motion.img
                key={`prev-${prevIndex}`}
                src={images[prevIndex]}
                alt=""
                className="tilted-card-img"
                initial={{ x: 0 }}
                animate={{ x: direction > 0 ? `-100%` : `100%` }}
                transition={{ type: "spring", stiffness: 70, damping: 20 }}
                style={{
                  width: "100%",
                  height: "100%",
                  objectFit: "cover",
                  position: "absolute",
                  top: 0,
                  left: 0,
                }}
              />
            )}

            <motion.img
              key={`curr-${currentIndex}`}
              src={images[currentIndex]}
              alt=""
              className="tilted-card-img"
              initial={{ x: direction > 0 ? `100%` : `-100%` }}
              animate={{ x: 0 }}
              transition={{ type: "spring", stiffness: 70, damping: 20 }}
              style={{
                width: "100%",
                height: "100%",
                objectFit: "cover",
                position: "absolute",
                top: 0,
                left: 0,
              }}
            />
          </div>

          {displayOverlayContent && overlayContent && (
            <motion.div className="tilted-card-overlay">
              {overlayContent}
            </motion.div>
          )}
        </motion.div>
      </figure>

      {/* Modal */}
      <Modal isOpen={isModalOpen} onClose={closeModal}>
      <AlbumDetails
        header= {captionText}
        content={modalContent}
        images={imageList}
        icons={svgIcons}
        link={{ href: "https://github.com/...", img: null, label: "Repo" }}
      />

      </Modal>
    </>
  );
}
