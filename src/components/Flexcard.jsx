import React from "react";
import PropTypes from "prop-types";

const FlexGrid = ({
  children,
  itemsPerRow = 4,
  gap = "16px",
  style = {},
  ...rest
}) => {
  const items = React.Children.toArray(children);
  const rows = [];
  for (let i = 0; i < items.length; i += itemsPerRow) {
    rows.push(items.slice(i, i + itemsPerRow));
  }

  return (
    <div
      {...rest}
      style={{ display: "flex", flexDirection: "column", gap, ...style }}
    >
      {rows.map((rowItems, rowIndex) => (
        <div
          key={rowIndex}
          style={{
            display: "flex",
            justifyContent: "space-between",
            gap,
          }}
        >
          {rowItems.map((child, idx) => (
            <div>{child}</div>
          ))}
        </div>
      ))}
    </div>
  );
};

FlexGrid.propTypes = {
  children: PropTypes.node.isRequired,
  itemsPerRow: PropTypes.number,
  gap: PropTypes.string,
  style: PropTypes.object,
};

export default FlexGrid;
