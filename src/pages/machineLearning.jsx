import { JupyterIcon, PythonIcon, PyTorchIcon } from "../components/IconComponent.jsx";
import TiltedCard from "../components/TitledCard.jsx";
import AlbumDetails from "./detailPages/machineLearningDetail.jsx"; 

const MachineLearning = () => {
    return (
    <TiltedCard
                imageList={[]}
                captionText="Machine Learning"
                svgIcons={[JupyterIcon, PyTorchIcon, PythonIcon]}
                hoverText="Click for details"
                modalContent={<AlbumDetails />}
              />
            );
};

export default MachineLearning;
