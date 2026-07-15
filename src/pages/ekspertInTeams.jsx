import TiltedCard from "../components/TitledCard.jsx";
import AlbumDetails from "./detailPages/ekspertInTeamsDetail.jsx"; 

const EkspertInTeams = () => {
    return (
    <TiltedCard
                imageList={[]}
                captionText="Ekspert in Teams"
                svgIcons={[]}
                hoverText="Click for details"
                modalContent={<AlbumDetails />}
              />
            );
};

export default EkspertInTeams;
