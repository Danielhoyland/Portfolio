import carmask from "../images/CarMask.jpg"
import fear from "../images/fearSad.png"
import suprise from "../images/suprise.png"
import Kmeans from "../images/k-means.png"
import tsne from "../images/tsne_plot.png"
import normConMat from "../images/normConMat.png"
import TiltedCard from "../components/TitledCard.jsx";
import {
  CollabIcon,
  JupyterIcon,
  PythonIcon,
  PyTorchIcon
} from "../components/IconComponent.jsx";
import AlbumDetails from "./detailPages/aiDetail.jsx"; 

const AI = () => {
    return (
    <TiltedCard
                imageList={[
                  carmask,
                  fear,
                  suprise,
                  Kmeans,
                  tsne,
                  normConMat
                ]}
                captionText="Artificial Inteligence"
                svgIcons={[CollabIcon, JupyterIcon, PythonIcon, PyTorchIcon]}
                hoverText="Click for details"
                modalContent={<AlbumDetails />}
              />
            );
};

export default AI;
