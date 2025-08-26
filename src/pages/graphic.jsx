
import OtherGame from "../images/OtherGame.gif"
import Chess from "../images/Chess.gif"
import TiltedCard from "../components/TitledCard.jsx";
import {
  OpenGLIcon,
  CPlusIcon,
  CMakeIcon,
} from "../components/IconComponent.jsx";
import AlbumDetails from "./detailPages/graphicDetail.jsx"; 

const Graphic = () => {
    return (
    <TiltedCard
                imageList={[
                  Chess,
                  OtherGame
                ]}
                captionText="Graphics Programming"
                svgIcons={[CPlusIcon, CMakeIcon, OpenGLIcon]}
                hoverText="Click for details"
                modalContent={<AlbumDetails />}
              />
            );
};

export default Graphic;
