import { JavaScriptIcon } from "../components/IconComponent.jsx";
import TiltedCard from "../components/TitledCard.jsx";
import AlbumDetails from "./detailPages/scriptBlackboardDetail.jsx"; 
import BB1 from "../images/BB_execution.png"
import BB2 from "../images/BB_script_runner.png"

const ScriptBlackboard = () => {
    return (
    <TiltedCard
                imageList={[BB1, BB2]}
                captionText="Script for Blackboard"
                svgIcons={[JavaScriptIcon]}
                hoverText="Click for details"
                modalContent={<AlbumDetails />}
              />
            );
};

export default ScriptBlackboard;
