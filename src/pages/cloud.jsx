import cloud0 from "../images/cloud0.png"
import cloud1 from "../images/cloud1.png"
import cloud2 from "../images/cloud2.png"
import TiltedCard from "../components/TitledCard.jsx";
import {
  GoIcon,
  PostIcon,
  FirebaseIcon
} from "../components/IconComponent.jsx";
import AlbumDetails from "./detailPages/cloudDetail.jsx"; 

const Cloud = () => {
    return (
    <TiltedCard
                imageList={[
                  cloud0,
                  cloud1,
                  cloud2
                ]}
                captionText="Cloud Technologies"
                svgIcons={[GoIcon, PostIcon, FirebaseIcon]}
                hoverText="Click for details"
                modalContent={<AlbumDetails />}
              />
            );
};

export default Cloud;
