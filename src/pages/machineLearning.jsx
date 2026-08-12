import { JupyterIcon, PythonIcon, PyTorchIcon } from "../components/IconComponent.jsx";
import TiltedCard from "../components/TitledCard.jsx";
import AlbumDetails from "./detailPages/machineLearningDetail.jsx"; 
import ITML1 from "../images/ITML1.png"
import ITML2 from "../images/ITML2.png"
import ITML3 from "../images/ITML3.png"
import ITML4 from "../images/ITML4.png"
import ITML5 from "../images/ITML5.png"
import ITML6 from "../images/ITML6.png"
import ITML7 from "../images/ITML7.png"
import ITML8 from "../images/ITML8.png"
import ITML9 from "../images/ITML9.png"

const MachineLearning = () => {
    return (
    <TiltedCard
                imageList={[ITML1, ITML2, ITML3, ITML4, ITML5, ITML6, ITML7, ITML8, ITML9]}
                captionText="Machine Learning"
                svgIcons={[JupyterIcon, PyTorchIcon, PythonIcon]}
                hoverText="Click for details"
                modalContent={<AlbumDetails />}
              />
            );
};

export default MachineLearning;
