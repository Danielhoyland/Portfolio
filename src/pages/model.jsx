import model0 from "../images/kidsmath.gif"
import model1 from "../images/mixedquiz.gif"
import model2 from "../images/lotr.gif"
import TiltedCard from "../components/TitledCard.jsx";
import {
  
} from "../components/IconComponent.jsx";
import AlbumDetails from "./detailPages/modelDetail.jsx"; 

const Model = () => {
    return (
    <TiltedCard
                imageList={[
                  model0,
                  model1,
                  model2
                ]}
                captionText="Model-Driven Software Engineering"
                svgIcons={[]}
                hoverText="Click for details"
                modalContent={<AlbumDetails />}
              />
            );
};

export default Model;
