import InkReader from "../images/InkReader.jpg";
import Quote from "../images/Quote.jpg";
import DndApp from "../images/DndApp.jpg";
import TiltedCard from "../components/TitledCard.jsx";
import {
  AndroidStudIcon,
  ExpoGoIcon,
  KotlinIcon,
  ReactNIcon,
} from "../components/IconComponent.jsx";
import AlbumDetails from "./detailPages/mobileDetail.jsx";

const Mobile = () => {
  return (
    <TiltedCard
      imageList={[InkReader, Quote, DndApp]}
      captionText="Mobile Programming"
      svgIcons={[AndroidStudIcon, ExpoGoIcon, KotlinIcon, ReactNIcon]}
      hoverText="Click for details"
      modalContent={<AlbumDetails />}
    />
  );
};

export default Mobile;
