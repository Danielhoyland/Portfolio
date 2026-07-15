import { JavaScriptIcon } from "../components/IconComponent.jsx";
import TiltedCard from "../components/TitledCard.jsx";
import AlbumDetails from "./detailPages/advancedSoftwareEngineeringDetail.jsx"; 
import advSoft1 from "../images/advSoft1.png"
import advSoft2 from "../images/advSoft2.png"

const AdvancedSoftwareEngineering = () => {
    return (
    <TiltedCard
                imageList={[advSoft1, advSoft2]}
                captionText="Advanced Software Engineering"
                svgIcons={[JavaScriptIcon]}
                hoverText="Click for details"
                modalContent={<AlbumDetails />}
              />
            );
};

export default AdvancedSoftwareEngineering;
