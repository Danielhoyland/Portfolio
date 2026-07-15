import TiltedCard from "../components/TitledCard.jsx";
import AlbumDetails from "./detailPages/appliedDataDetail.jsx"; 

const AppliedData = () => {
    return (
    <TiltedCard
                imageList={[]}
                captionText="Applied Data"
                svgIcons={[]}
                hoverText="Click for details"
                modalContent={<AlbumDetails />}
              />
            );
};

export default AppliedData;
