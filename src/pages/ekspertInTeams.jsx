import { MarsIcon } from "lucide-react";
import { GoIcon, MySqlIcon, ReactIcon, TypeScriptIcon } from "../components/IconComponent.jsx";
import TiltedCard from "../components/TitledCard.jsx";
import AlbumDetails from "./detailPages/ekspertInTeamsDetail.jsx"; 

const EkspertInTeams = () => {
    return (
    <TiltedCard
                imageList={[]}
                captionText="Ekspert in Teams"
                svgIcons={[ReactIcon, MySqlIcon, MarsIcon, GoIcon, TypeScriptIcon]}
                hoverText="Click for details"
                modalContent={<AlbumDetails />}
              />
            );
};

export default EkspertInTeams;
