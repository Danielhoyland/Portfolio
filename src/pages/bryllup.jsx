import { ReactIcon } from "../components/IconComponent.jsx";
import TiltedCard from "../components/TitledCard.jsx";
import AE_Desktop from "../images/AE_Desktop.gif"
import AlbumDetails from "./detailPages/bryllopDetail.jsx"; 

const Bryllup = () => {
    return (
    <TiltedCard
                imageList={[AE_Desktop
                ]}
                captionText="Bryllup"
                svgIcons={[ReactIcon]}
                hoverText="Click for details"
                modalContent={<AlbumDetails />}
              />
            );
};

export default Bryllup;
