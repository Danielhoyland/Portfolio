import { PythonIcon } from "../components/IconComponent.jsx";
import TiltedCard from "../components/TitledCard.jsx";
import AlbumDetails from "./detailPages/notificationDiscordDetail.jsx"; 

const NotificationDiscord = () => {
    return (
    <TiltedCard
                imageList={[]}
                captionText="Notification Integration with Discord"
                svgIcons={[PythonIcon]}
                hoverText="Click for details"
                modalContent={<AlbumDetails />}
              />
            );
};

export default NotificationDiscord;
