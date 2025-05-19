import React from "react";
import { Navigate } from "react-router-dom";
import { useAuth } from "../../context/AuthContext";
import { isTokenExpired } from "../../services/userService";

const ProtectedRoute = ({ children }) => {
  const { tokenLog } = useAuth()
  if (!tokenLog || isTokenExpired(tokenLog)) {
    return <Navigate to="/" />;
  }

  return children;
};

export default ProtectedRoute;
