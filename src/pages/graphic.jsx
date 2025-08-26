
import OtherGame from "../images/OtherGame.gif"
import Chess from "../images/Chess.gif"
import TiltedCard from "../components/TitledCard.jsx";
import {
  CollabIcon,
  JupyterIcon,
  PythonIcon,
  PyTorchIcon
} from "../components/IconComponent.jsx";
import AlbumDetails from "./detailPages/gameDetail.jsx"; 

const Graphic = () => {
    return (
    <TiltedCard
                imageList={[
                  Chess,
                  OtherGame
                ]}
                captionText="Graphics Programming"
                svgIcons={[CollabIcon, JupyterIcon, PythonIcon, PyTorchIcon]}
                hoverText="Click for details"
                modalContent={<AlbumDetails />}
              />
            );
};

export default Graphic;
