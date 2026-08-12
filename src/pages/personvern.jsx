import TiltedCard from "../components/TitledCard.jsx";
import AlbumDetails from "./detailPages/personvernDetail.jsx"; 
import WIP from "../images/WIP.png"
const Personvern = () => {
    return (
    <TiltedCard
                imageList={[WIP]}
                captionText="Personvern"
                svgIcons={[]}
                hoverText="Click for details"
                modalContent={<AlbumDetails />}
              />
            );
};

export default Personvern;
