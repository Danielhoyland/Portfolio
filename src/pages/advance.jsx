
import Interperter from "../images/Interperter.png"
import go from "../images/GO.png"
import TiltedCard from "../components/TitledCard.jsx";
import {
  HaskellIcon,
  RustIcon,
  SDL2Icon,
  GoIcon
} from "../components/IconComponent.jsx";
import AlbumDetails from "./detailPages/advanceDetail.jsx"; 

const Advance = () => {
    return (
    <TiltedCard
                imageList={[
                  go,
                  Interperter,

                ]}
                captionText="Advance Programming"
                svgIcons={[HaskellIcon, RustIcon, SDL2Icon, GoIcon]}
                hoverText="Click for details"
                modalContent={<AlbumDetails />}
              />
            );
};

export default Advance;
