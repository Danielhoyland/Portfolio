import FridgeIO from "../images/FridgeIO.jpg"
import mariaDBStruktur from "../images/mariaDBStruktur.png"
import HomescreenWireframe from "../images/HomescreenWireframe.png"
import TiltedCard from "../components/TitledCard.jsx";
import {
  CollabIcon,
  JupyterIcon,
  PythonIcon,
  PyTorchIcon
} from "../components/IconComponent.jsx";
import AlbumDetails from "./detailPages/integrationDetail.jsx"; 

const Inte = () => {
    return (
    <TiltedCard
                imageList={[
                  FridgeIO,
                  mariaDBStruktur,
                  HomescreenWireframe
                ]}
                captionText="Integration Project"
                svgIcons={[CollabIcon, JupyterIcon, PythonIcon, PyTorchIcon]}
                hoverText="Click for details"
                modalContent={<AlbumDetails />}
              />
            );
};

export default Inte;
