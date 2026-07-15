import TiltedCard from "../components/TitledCard.jsx";
import AlbumDetails from "./detailPages/bryllopDetail.jsx"; 

const Bryllup = () => {
    return (
    <TiltedCard
                imageList={[]}
                captionText="Bryllup"
                svgIcons={[]}
                hoverText="Click for details"
                modalContent={<AlbumDetails />}
              />
            );
};

export default Bryllup;
