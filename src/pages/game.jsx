import game from "../images/game.png"
import Mechanic from "../images/Mechanic.png"
import TiltedCard from "../components/TitledCard.jsx";
import {
  C,
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
