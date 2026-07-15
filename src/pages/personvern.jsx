import TiltedCard from "../components/TitledCard.jsx";
import AlbumDetails from "./detailPages/personvernDetail.jsx"; 

const Personvern = () => {
    return (
    <TiltedCard
                imageList={[]}
                captionText="Personvern"
                svgIcons={[]}
                hoverText="Click for details"
                modalContent={<AlbumDetails />}
              />
            );
};

export default Personvern;
