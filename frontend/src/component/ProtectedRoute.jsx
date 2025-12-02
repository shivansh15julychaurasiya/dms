// src/routes/PrivateRoute.jsx
import { useAuth } from "../context/AuthContext";
import { Navigate } from "react-router-dom";

export default function PrivateRoute({ children }) {
  const { user, loading } = useAuth();

  if (loading) {
    return <p className="text-center mt-5">Checking authentication...</p>;
  }

  return user ? children : <Navigate to="/" replace />;
}
