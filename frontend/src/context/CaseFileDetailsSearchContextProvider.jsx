// CaseFileDetailsSearchContextProvider.jsx

import React, { createContext, useContext, useState } from "react";

// Create context
const SearchResultContext = createContext();

// Provider component
export const CaseFileDetailsSearchContextProvider = ({ children }) => {
  const [searchResult, setSearchResult] = useState(null);

  return (
    <SearchResultContext.Provider value={{ searchResult, setSearchResult }}>
      {children}
    </SearchResultContext.Provider>
  );
};

// Custom hook
export const useSearchResult = () => useContext(SearchResultContext);
