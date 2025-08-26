
import bachelor from "../images/bachelor.jpg"
import product from "../images/bachelor-overview.png"
import TiltedCard from "../components/TitledCard.jsx";
import {
  CollabIcon,
  JupyterIcon,
  PythonIcon,
  PyTorchIcon
} from "../components/IconComponent.jsx";
import AlbumDetails from "./detailPages/bachelorDetail.jsx"; 

const Bach = () => {
    return (
    <TiltedCard
                imageList={[
                  bachelor,
                  product,
                ]}
                captionText="Bachelor thesis"
                svgIcons={[CollabIcon, JupyterIcon, PythonIcon, PyTorchIcon]}
                hoverText="Click for details"
                modalContent={<AlbumDetails />}
              />
            );
};

export default Bach;
