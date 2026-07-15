import TiltedCard from "../components/TitledCard.jsx";
import AlbumDetails from "./detailPages/informationRetrievalDetail.jsx"; 

const InformationRetrieval = () => {
    return (
    <TiltedCard
                imageList={[]}
                captionText="Information Retrieval"
                svgIcons={[]}
                hoverText="Click for details"
                modalContent={<AlbumDetails />}
              />
            );
};

export default InformationRetrieval;
