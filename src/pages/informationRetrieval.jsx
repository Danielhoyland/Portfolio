import TiltedCard from "../components/TitledCard.jsx";
import AlbumDetails from "./detailPages/informationRetrievalDetail.jsx"; 
import IR1 from "../images/IR1.png"
import IR2 from "../images/IR2.png"
import IR3 from "../images/IR3.png"
import IR4 from "../images/IR4.png"

import {
  CollabIcon,
  JupyterIcon,
  PythonIcon,
} from "../components/IconComponent.jsx";

const InformationRetrieval = () => {
    return (
    <TiltedCard
                imageList={[IR1, IR2, IR3, IR4]}
                captionText="Information Retrieval"
                svgIcons={[CollabIcon, JupyterIcon, PythonIcon]}
                hoverText="Click for details"
                modalContent={<AlbumDetails />}
              />
            );
};

export default InformationRetrieval;
