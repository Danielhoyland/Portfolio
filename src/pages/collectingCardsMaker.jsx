import TiltedCard from "../components/TitledCard.jsx";
import AlbumDetails from "./detailPages/collectingCardsMakerDetail.jsx"; 
import collect1 from "../images/card_collect_example.jpg"
import collect2 from "../images/Card_collecting_execution.png"
import {
  PythonIcon
} from "../components/IconComponent.jsx";

const CollectingCardsMaker = () => {
    return (
    <TiltedCard
                imageList={[collect1, collect2]}
                captionText="Collecting Cards Maker"
                svgIcons={[PythonIcon]}
                hoverText="Click for details"
                modalContent={<AlbumDetails />}
              />
            );
};

export default CollectingCardsMaker;
