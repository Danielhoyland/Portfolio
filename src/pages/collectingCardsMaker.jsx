import TiltedCard from "../components/TitledCard.jsx";
import AlbumDetails from "./detailPages/collectingCardsMakerDetail.jsx"; 

const CollectingCardsMaker = () => {
    return (
    <TiltedCard
                imageList={[]}
                captionText="Collecting Cards Maker"
                svgIcons={[]}
                hoverText="Click for details"
                modalContent={<AlbumDetails />}
              />
            );
};

export default CollectingCardsMaker;
