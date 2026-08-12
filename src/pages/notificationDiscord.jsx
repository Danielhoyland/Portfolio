import { PythonIcon } from "../components/IconComponent.jsx";
import TiltedCard from "../components/TitledCard.jsx";
import AlbumDetails from "./detailPages/notificationDiscordDetail.jsx"; 
import notificationMessage from "../images/notificationDisc.png"
import serverRunner from "../images/notficationDiscRunner.png"
import cronTap from "../images/notificationCronTap.png"


const NotificationDiscord = () => {
    return (
    <TiltedCard
                imageList={[serverRunner, cronTap, notificationMessage]}
                captionText="Website change monitoring system"
                svgIcons={[PythonIcon]}
                hoverText="Click for details"
                modalContent={<AlbumDetails />}
              />
            );
};

export default NotificationDiscord;
