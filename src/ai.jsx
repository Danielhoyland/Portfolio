import carmask from "./images/CarMask.jpg"
import fear from "./images/fearSad.png"
import suprise from "./images/suprise.png"
import TiltedCard from "./components/TitledCard";
import {
  AxiosIcon,
  CMakeIcon,
  CPPIcon,
  DockerIcon,
  GitHubIcon,
  GoIcon,
  GodotIcon,
  NginXIcon,
  NtnuIcon,
  OpenGLIcon,
  ReactIcon,
  RenderDocIcon,
  SqliteIcon,
  TailwindIcon,
  TypeScriptIcon,
  ViteIcon,
} from "./components/IconComponent.jsx";
import AlbumDetails from "./detailPages/aiDetail.jsx"; 

const AI = () => {
    return (
    <TiltedCard
                imageList={[
                  carmask,
                  "https://i.scdn.co/image/ab67616d0000b273d9985092cd88bffd97653b58",
                ]}
                captionText="Artificial Inteligence"
                svgIcons={[DockerIcon, DockerIcon]}
                hoverText="Click for details"
                modalContent={<AlbumDetails />}
              />
            );
};

export default AI;