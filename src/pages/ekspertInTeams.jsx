import { GoIcon, MySqlIcon, ReactIcon, TypeScriptIcon } from "../components/IconComponent.jsx";
import TiltedCard from "../components/TitledCard.jsx";
import AlbumDetails from "./detailPages/ekspertInTeamsDetail.jsx"; 
import eit1 from "../images/Plant_pod_assembly.png";
import eit2 from "../images/prototype_bilde.jpg";
import eit3 from "../images/React_Dashboard_API.png";
import eit4 from "../images/PlantPodPOSTReq.png";
import eit5 from "../images/endpoints.png";
import eit6 from "../images/enpointsOverview.png";
import eit7 from "../images/DashboardPlantPod.png";
import eit8 from "../images/Database.png";

const EkspertInTeams = () => {
    return (
    <TiltedCard
                imageList={[eit1, eit2, eit3, eit4, eit5, eit6, eit8, eit7]}
                captionText="Ekspert in Teams"
                svgIcons={[ReactIcon, MySqlIcon, GoIcon, TypeScriptIcon]}
                hoverText="Click for details"
                modalContent={<AlbumDetails />}
              />
            );
};

export default EkspertInTeams;
