import TiltedCard from "../components/TitledCard.jsx";
import AlbumDetails from "./detailPages/scriptBlackboardDetail.jsx"; 

const ScriptBlackboard = () => {
    return (
    <TiltedCard
                imageList={[]}
                captionText="Script for Blackboard"
                svgIcons={[]}
                hoverText="Click for details"
                modalContent={<AlbumDetails />}
              />
            );
};

export default ScriptBlackboard;
