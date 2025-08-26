import carmask from "../images/CarMask.jpg"
import fear from "../images/fearSad.png"
import suprise from "../images/suprise.png"
import Kmeans from "../images/k-means.png"
import game from "../images/game.png"
import Mechanic from "../images/Mechanic.png"
import TiltedCard from "../components/TitledCard.jsx";
import {
  CollabIcon,
  JupyterIcon,
  PythonIcon,
  PyTorchIcon
} from "../components/IconComponent.jsx";
import AlbumDetails from "./detailPages/gameDetail.jsx"; 

const Game = () => {
    return (
    <TiltedCard
                imageList={[
                  Mechanic,
                  game,
                  
                ]}
                captionText="Game Programming"
                svgIcons={[CollabIcon, JupyterIcon, PythonIcon, PyTorchIcon]}
                hoverText="Click for details"
                modalContent={<AlbumDetails />}
              />
            );
};

export default Game;
