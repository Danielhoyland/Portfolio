// DataContext.js
import React, { createContext, useContext, useReducer } from 'react';

const DataContext = createContext();

export const AppData = ({ children }) => {
  const initialState = {
    user: null,
    inventory: null,
  };

  const [data, dispatch] = useReducer(dataReducer, initialState);

  return (
    <DataContext.Provider value={{ data, dispatch }}>{children}</DataContext.Provider>
  );
};

export const useData = () => {
  return useContext(DataContext);
};

const dataReducer = (state, action) => {
  switch (action.type) {
    case 'SET_USER':
      return { ...state, user: action.payload };
    case 'SET_INVENTORY':
      return { ...state, inventory: action.payload };
    default:
      return state;
  }
};