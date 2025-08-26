import carmask from "../images/CarMask.jpg"
import fear from "../images/fearSad.png"
import suprise from "../images/suprise.png"
import Kmeans from "../images/k-means.png"
import tsne from "../images/tsne_plot.png"
import cloud0 from "../images/cloud0.png"
import cloud1 from "../images/cloud1.png"
import cloud2 from "../images/cloud2.png"
import TiltedCard from "../components/TitledCard.jsx";
import {
  CollabIcon,
  JupyterIcon,
  PythonIcon,
  PyTorchIcon
} from "../components/IconComponent.jsx";
import AlbumDetails from "./detailPages/cloudDetail.jsx"; 

const Cloud = () => {
    return (
    <TiltedCard
                imageList={[
                  cloud0,
                  cloud1,
                  cloud2
                ]}
                captionText="Cloud Technologies"
                svgIcons={[CollabIcon, JupyterIcon, PythonIcon, PyTorchIcon]}
                hoverText="Click for details"
                modalContent={<AlbumDetails />}
              />
            );
};

export default Cloud;
