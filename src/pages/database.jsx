import carmask from "../images/CarMask.jpg"
import fear from "../images/fearSad.png"
import suprise from "../images/suprise.png"
import Kmeans from "../images/k-means.png"
import Python from "../images/Python.png"
import erd from "../images/ERD.png"
import TiltedCard from "../components/TitledCard.jsx";
import {
  CollabIcon,
  JupyterIcon,
  PythonIcon,
  PyTorchIcon
} from "../components/IconComponent.jsx";
import AlbumDetails from "./detailPages/databaseDetail.jsx"; 

const DB = () => {
    return (
    <TiltedCard
                imageList={[
                  Python,
                  erd
                ]}
                captionText="Data Modelling, Databases and Database Management Systems"
                svgIcons={[CollabIcon, JupyterIcon, PythonIcon, PyTorchIcon]}
                hoverText="Click for details"
                modalContent={<AlbumDetails />}
              />
            );
};

export default DB;
