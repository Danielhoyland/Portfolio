import bachelor from "../images/bachelor.jpg";
import product from "../images/bachelor-overview.png";
import TiltedCard from "../components/TitledCard.jsx";
import {
  DockerIcon,
  GoIcon,
  ReactIcon,
  ViteIcon,
  MySqlIcon,
  ChirpStackIcon,
  ShadcnIcon,
  WisgateIcon,
} from "../components/IconComponent.jsx";
import AlbumDetails from "./detailPages/bachelorDetail.jsx";

const Bach = () => {
  return (
    <TiltedCard
      imageList={[bachelor, product]}
      captionText="Bachelor thesis"
      svgIcons={[
        DockerIcon,
        ViteIcon,
        ReactIcon,
        GoIcon,
        MySqlIcon,
        ChirpStackIcon,
        ShadcnIcon,
        WisgateIcon,
      ]}
      hoverText="Click for details"
      modalContent={<AlbumDetails />}
    />
  );
};

export default Bach;
