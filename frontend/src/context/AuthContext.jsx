import React, { createContext, useState, useEffect } from "react";
import { isTokenExpired } from "../services/userService";

export const AuthContext = createContext();

export const AuthProvider = ({ children }) => {

  const [tokenLog, setToken] = useState(localStorage.getItem("tokenLog") || "");

  const [user, setUser] = useState(() => {
    try {
      return JSON.parse(localStorage.getItem("user")) || null;
    } catch (e) {
      console.error("Error parsing user data:", e);
      return null;
    }
  });
  const [role, setRole] = useState(localStorage.getItem("role") || "");

  // Check for tokenLog expiration when tokenLog is available
  useEffect(() => {
    if (tokenLog && isTokenExpired(tokenLog)) {
      logout();
    }
  }, [tokenLog]);

  // Syncing state with localStorage
 // Syncing state with localStorage
useEffect(() => {
  if (tokenLog && user) {
    localStorage.setItem("tokenLog", tokenLog);
    localStorage.setItem("user", JSON.stringify(user));

    // Check if the roles array exists and has at least one role before accessing it
    if (user.roles && user.roles[0] && user.roles[0].role_name) {
      setRole(user.roles[0].role_name.trim());
      localStorage.setItem("role", user.roles[0].role_name.trim()); //  Corrected here
    } else {
      setRole(""); // Clear role if no valid role found
    }
  }
}, [tokenLog, user]);


  const login = ({ tokenLog, user }) => {
    setToken(tokenLog);
    setUser(user);

    // Check if the roles array exists and has at least one role
    if (user.roles && user.roles[0] && user.roles[0].role_name) {
      setRole(user.roles[0].role_name.trim());
    } else {
      setRole(""); // Clear role if no valid role found
    }
  };

  const logout = () => {
    setToken("");
    setUser(null);
    setRole("");
    localStorage.clear();
    localStorage.setItem("logout", Date.now()); // Trigger cross-tab logout
  };

  return (
    <AuthContext.Provider value={{ tokenLog, user, role, login, logout }}>
      {children}
    </AuthContext.Provider>
  );
};

// Custom hook to access Auth context
export const useAuth = () => React.useContext(AuthContext);
