import FridgeIO from "../images/FridgeIO.jpg"
import mariaDBStruktur from "../images/mariaDBStruktur.png"
import HomescreenWireframe from "../images/HomescreenWireframe.png"
import TiltedCard from "../components/TitledCard.jsx";
import {
  ReactIcon,
  ViteIcon,
  DockerIcon,
  GoIcon,
  SqliteIcon
} from "../components/IconComponent.jsx";
import AlbumDetails from "./detailPages/integrationDetail.jsx"; 

const Inte = () => {
    return (
    <TiltedCard
                imageList={[
                  FridgeIO,
                  mariaDBStruktur,
                  HomescreenWireframe
                ]}
                captionText="Integration Project"
                svgIcons={[ReactIcon, ViteIcon, DockerIcon, GoIcon, SqliteIcon]}
                hoverText="Click for details"
                modalContent={<AlbumDetails />}
              />
            );
};

export default Inte;
