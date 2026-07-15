import React from 'react';
import Modal from 'react-modal';


interface PopoutScreenProps {
  isOpen: boolean;
  onClose: () => void;
  content: React.ReactNode;
}

const Popout: React.FC<PopoutScreenProps> = ({ isOpen, onClose, content }) => {
    
  return (
    <Modal
      isOpen={isOpen}
      onRequestClose={onClose}
      shouldCloseOnOverlayClick={true}
      shouldCloseOnEsc={true}
      style={{
        content: {
          width: 'max-content',
          height: 'max-content',
          margin: 'auto',
        },
    }}
    >
      {content}
    </Modal>
  );
};

export default Popout;
