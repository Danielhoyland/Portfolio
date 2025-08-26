
import Interperter from "../images/Interperter.png"
import go from "../images/GO.png"
import TiltedCard from "../components/TitledCard.jsx";
import {
  CollabIcon,
  JupyterIcon,
  PythonIcon,
  PyTorchIcon
} from "../components/IconComponent.jsx";
import AlbumDetails from "./detailPages/advanceDetail.jsx"; 

const Advance = () => {
    return (
    <TiltedCard
                imageList={[
                  go,
                  Interperter,

                ]}
                captionText="Advance Programming"
                svgIcons={[CollabIcon, JupyterIcon, PythonIcon, PyTorchIcon]}
                hoverText="Click for details"
                modalContent={<AlbumDetails />}
              />
            );
};

export default Advance;
