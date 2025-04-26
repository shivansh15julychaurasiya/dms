import React from "react";
import { Navigate } from "react-router-dom";
import { useAuth } from "../../context/AuthContext";
import { isTokenExpired } from "../../services/axios";

const ProtectedRoute = ({ children }) => {
  const { token } = useAuth()
  if (!token || isTokenExpired(token)) {
    return <Navigate to="/home/login" />;
  }

  return children;
};

export default ProtectedRoute;
