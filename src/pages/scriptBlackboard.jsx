import { JavaScriptIcon } from "../components/IconComponent.jsx";
import TiltedCard from "../components/TitledCard.jsx";
import AlbumDetails from "./detailPages/scriptBlackboardDetail.jsx"; 

const ScriptBlackboard = () => {
    return (
    <TiltedCard
                imageList={[]}
                captionText="Script for Blackboard"
                svgIcons={[JavaScriptIcon]}
                hoverText="Click for details"
                modalContent={<AlbumDetails />}
              />
            );
};

export default ScriptBlackboard;
