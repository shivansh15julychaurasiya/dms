import React from "react";
import { Navigate } from "react-router-dom";
import { useAuth } from "../../context/AuthContext";
import { isTokenExpired } from "../../services/userService";


//    Fullstack Java Developer Vijay Chaurasiya

const ProtectedRoute = ({ children }) => {
  const { token } = useAuth()
  if (!token || isTokenExpired(token)) {
    return <Navigate to="/" />;
  }

  return children;
};

export default ProtectedRoute;
