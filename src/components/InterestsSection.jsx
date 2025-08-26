import React from 'react';
import RotatingText from './RotatingText';
import './RotatingText.css'; 

const InterestsSection = ({ interests, Interval }) => {
  return (
    <div className="interests-section">
      <RotatingText
        texts={interests}
        mainClassName="px-4 py-2 rounded-2xl font-semibold text-lg shadow-md"
        staggerFrom="first"
        initial={{ y: '100%', opacity: 0 }}
        animate={{ y: 0, opacity: 1 }}
        exit={{ y: '-120%', opacity: 0 }}
        staggerDuration={0.04}
        splitLevelClassName="overflow-hidden"
        transition={{ type: 'spring', damping: 25, stiffness: 300 }}
        rotationInterval={Interval}
        style={{
          backgroundColor: '#00509E',   
          color: '#ffffff',             
          padding: '6px 12px',
          fontWeight: '600',
          border: '2px solid #3e628a', 
          borderRadius: '16px',
          justifyContent: 'center',
          alignItems: 'center',
          boxShadow: '0 4px 8px rgba(0, 80, 158, 0.15)',
          width: 'clamp(180px, 20vw, 350px)',
        }}
      />
    </div>
  );
};

export default InterestsSection;
