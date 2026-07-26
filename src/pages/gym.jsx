import { KotlinIcon, SqliteIcon } from "../components/IconComponent.jsx";
import TiltedCard from "../components/TitledCard.jsx";
import AlbumDetails from "./detailPages/gymDetail.jsx"; 

const Gym = () => {
    return (
    <TiltedCard
                imageList={[]}
                captionText="Gym tracker APP"
                svgIcons={[SqliteIcon, KotlinIcon]}
                hoverText="Click for details"
                modalContent={<AlbumDetails />}
              />
            );
};

export default Gym;
