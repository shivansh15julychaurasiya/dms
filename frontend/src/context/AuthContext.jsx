import React, { createContext, useState, useEffect } from "react";
import { isTokenExpired } from "../services/userService";

export const AuthContext = createContext();

export const AuthProvider = ({ children }) => {
  const [token, setToken] = useState(localStorage.getItem("token") || "");

  const [user, setUser] = useState(() => {
    try {
      return JSON.parse(localStorage.getItem("user")) || null;
    } catch (e) {
      console.error("Error parsing user data:", e);
      return null;
    }
  });
  const [role, setRole] = useState(localStorage.getItem("role") || "");

  // Check for token expiration when token is available
  useEffect(() => {
    if (token && isTokenExpired(token)) {
      logout();
    }
  }, [token]);

  // Syncing state with localStorage
  // Syncing state with localStorage
  useEffect(() => {
    if (token && user) {
      localStorage.setItem("token", token);
      localStorage.setItem("user", JSON.stringify(user));

      // Check if the roles array exists and has at least one role before accessing it
      if (user.roles && user.roles[0] && user.roles[0].role_name) {
        setRole(user.roles[0].role_name.trim());
        localStorage.setItem("role", user.roles[0].role_name.trim()); //  Corrected here
      } else {
        setRole(""); // Clear role if no valid role found
      }
    }
  }, [token, user]);

  const login = ({ token, user }) => {
    setToken(token);
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
    <AuthContext.Provider value={{ token, user, role, login, logout }}>
      {children}
    </AuthContext.Provider>
  );
};

// Custom hook to access Auth context
export const useAuth = () => React.useContext(AuthContext);
