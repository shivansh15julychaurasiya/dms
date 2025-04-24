import React, { useEffect } from "react";
import { BrowserRouter, Routes, Route } from "react-router-dom";
import 'react-toastify/dist/ReactToastify.css';
import "bootstrap/dist/css/bootstrap.min.css";
import "bootstrap-icons/font/bootstrap-icons.css";

import Home from "./pages/home/Home";
import Login from "./components/auth/Login";
import Register from "./components/auth/Register";
import ForgotPassword from "./components/auth/ForgotPassword";
import ResetPassword from "./components/auth/ResetPassword";
import Dashboard from "./components/dashboard/DashboardLayout";
import AdminDashboard from "./components/dashboard/AdminDashboard";
import UserDashboard from "./components/dashboard/UserDashboard";
import Search from "./components/layout/Search";
import EditProfile from "./components/profile/EditProfile";
import ManageCauseList from "./components/ManageCauseList";
import PDFViewer from "./components/pdf/PDFViewer";
import UnAuthorize from "./components/UnAuthorize";

import ProtectedRoute from "./components/auth/ProtectedRoute"; // 

export default function App() {



  useEffect(() => {
    const handleStorageChange = (event) => {
      if (event.key === "logout") {
        localStorage.removeItem("token");
        window.location.href = "/dms/home/login";
      }
    };
    window.addEventListener("storage", handleStorageChange);
    return () => window.removeEventListener("storage", handleStorageChange);
  }, []);

  return (
    
    <BrowserRouter basename="/dms">
      <Routes>
        {/* Public Routes */}
        <Route path="" element={<Home />} />
        <Route path="/home/login" element={<Login />} />
        <Route path="/home/register" element={<Register />} />
        <Route path="/home/forgot" element={<ForgotPassword />} />
        <Route path="/home/reset" element={<ResetPassword />} />

        {/* Protected Routes */}
        <Route
          path="/home/dashboard"
          element={
            <ProtectedRoute>
              <Dashboard />
            </ProtectedRoute>
          }
        />
        <Route
          path="/home/admindashboard"
          element={
            <ProtectedRoute>
              <AdminDashboard />
            </ProtectedRoute>
          }
        />
        <Route
          path="/home/userdashboard"
          element={
            <ProtectedRoute>
              <UserDashboard />
            </ProtectedRoute>
          }
        />
        <Route
          path="/home/search"
          element={
            <ProtectedRoute>
              <Search />
            </ProtectedRoute>
          }
        />
        <Route
          path="/home/editprofile"
          element={
            <ProtectedRoute>
              <EditProfile />
            </ProtectedRoute>
          }
        />
        <Route
          path="/home/managecauselist"
          element={
            <ProtectedRoute>
              <ManageCauseList />
            </ProtectedRoute>
          }
        />
        <Route
          path="/pdf"
          element={
            <ProtectedRoute>
              <PDFViewer />
            </ProtectedRoute>
          }
        />

        {/* Unauthorized page */}
        <Route path="/home/unauthorize" element={<UnAuthorize />} />
      </Routes>
    </BrowserRouter>
  );
}
