import React from 'react';
import RotatingText from './RotatingText';
import './RotatingText.css'; 

const InterestsSection = ({ interests, Interval }) => {
  return (
    <div className="interests-section">
        <RotatingText
          texts={interests}
          mainClassName="inline-block px-4 py-2 rounded-2xl font-semibold text-lg shadow-md text-black"
          staggerFrom="first"
          initial={{ y: '100%', opacity: 0 }}
          animate={{ y: 0, opacity: 1 }}
          exit={{ y: '-120%', opacity: 0 }}
          staggerDuration={0.04}
          splitLevelClassName="overflow-hidden"
          transition={{ type: 'spring', damping: 25, stiffness: 300 }}
          rotationInterval={Interval}
          style={{
            backgroundColor: '#fff0c6',
            borderStyle: 'solid',
            borderColor: '#b74706',
            borderRadius: '16px',
            borderWidth: '2px',
            justifyContent: 'center',
        }}
        />
    </div>
  );
};



export default InterestsSection;
