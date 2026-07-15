import TiltedCard from "../components/TitledCard.jsx";
import AlbumDetails from "./detailPages/gymDetail.jsx"; 

const Gym = () => {
    return (
    <TiltedCard
                imageList={[]}
                captionText="Gym"
                svgIcons={[]}
                hoverText="Click for details"
                modalContent={<AlbumDetails />}
              />
            );
};

export default Gym;
