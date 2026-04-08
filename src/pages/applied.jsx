import applied0 from "../images/applied0.png"
import applied1 from "../images/applied1.png"
import applied2 from "../images/applied2.png"
import applied3 from "../images/applied3.png"
import TiltedCard from "../components/TitledCard.jsx";
import {
  CollabIcon,
  JupyterIcon,
  PythonIcon,
} from "../components/IconComponent.jsx";
import AlbumDetails from "./detailPages/appliedDetail.jsx"; 

const Applied = () => {
    return (
    <TiltedCard
                imageList={[
                  applied0,
                  applied1,
                  applied2,
                  applied3
                ]}
                captionText="Applied Data Science"
                svgIcons={[CollabIcon, JupyterIcon, PythonIcon]}
                hoverText="Click for details"
                modalContent={<AlbumDetails />}
              />
            );
};

export default Applied;
