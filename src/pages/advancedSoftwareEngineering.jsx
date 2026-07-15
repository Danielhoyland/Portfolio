import TiltedCard from "../components/TitledCard.jsx";
import AlbumDetails from "./detailPages/advancedSoftwareEngineeringDetail.jsx"; 

const AdvancedSoftwareEngineering = () => {
    return (
    <TiltedCard
                imageList={[]}
                captionText="Advanced Software Engineering"
                svgIcons={[]}
                hoverText="Click for details"
                modalContent={<AlbumDetails />}
              />
            );
};

export default AdvancedSoftwareEngineering;
