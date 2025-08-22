import React from "react";
import PropTypes from "prop-types";
import "./FlexGrid.css";

const FlexGrid = ({ children, gap = "16px", style = {}, ...rest }) => {
  return (
    <div
      {...rest}
      style={{
        gap,
        ...style,
      }}
      className="flex-grid"
    >
      {React.Children.toArray(children)}
    </div>
  );
};

FlexGrid.propTypes = {
  children: PropTypes.node.isRequired,
  gap: PropTypes.string,
  style: PropTypes.object,
};

export default FlexGrid;
