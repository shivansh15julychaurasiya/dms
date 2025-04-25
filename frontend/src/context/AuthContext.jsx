import React, { createContext, useState, useEffect } from "react";
import { isTokenExpired } from "../services/axios";

export const AuthContext = createContext();

export const AuthProvider = ({ children }) => {
  const [token, setToken] = useState(localStorage.getItem("token") || "");
  const [user, setUser] = useState(
    JSON.parse(localStorage.getItem("user")) || null
  );
  const [role, setRole] = useState(localStorage.getItem("role") || "");

  useEffect(() => {
    if (token && isTokenExpired(token)) {
      logout();
    }
  }, []);

  useEffect(() => {
    localStorage.setItem("token", token || "");
    localStorage.setItem("user", JSON.stringify(user) || "");
    localStorage.setItem("role", role || "");
  }, [token, user, role]);

  const login = ({ token, user }) => {
    setToken(token);
    setUser(user);
    setRole(user.roles[0].name.trim());
  };

  const logout = () => {
    setToken("");
    setUser(null);
    setRole("");
    localStorage.clear();
    localStorage.setItem("logout", Date.now()); // trigger cross-tab logout
  };

  return (
    <AuthContext.Provider value={{ token, user, role, login, logout }}>
      {children}
    </AuthContext.Provider>
  );
};
// Add this at the bottom of your file
export const useAuth = () => React.useContext(AuthContext);

