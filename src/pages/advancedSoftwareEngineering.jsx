import { JavaScriptIcon } from "../components/IconComponent.jsx";
import TiltedCard from "../components/TitledCard.jsx";
import AlbumDetails from "./detailPages/advancedSoftwareEngineeringDetail.jsx"; 
import {advSoft1, advSoft2} from "../images"

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
