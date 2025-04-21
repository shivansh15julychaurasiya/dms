import React, { useEffect } from "react";
import { BrowserRouter, Routes, Route } from "react-router-dom";

import Home from "./Pages/home/Home";
import Login from "./components/Login";
import Search from "./components/Search";
import Dashboard from "./components/DashboardLayout";
import PDFViewer from "./components/pdf/PDFViewer";
import ForgotPassword from "./components/ForgotPassword";
import EditProfile from "./components/EditProfile";
import ManageCauseList from "./components/ManageCauseList";
import Register from "./components/Register";
import UserDashboard from "./components/UserDashboard";
import ResetPassword from "./components/ResetPassword";

export default function App() {

  useEffect(() => {
    const handleStorageChange = (event) => {
      if (event.key === "logout") {
        // Clear token just in case
        localStorage.removeItem("token");
        // Redirect user to login page
        window.location.href = "/dms/home/login";
      }
    };

    window.addEventListener("storage", handleStorageChange);

    return () => {
      window.removeEventListener("storage", handleStorageChange);
    };
  }, []);

  return (
    <div className="App">
      <BrowserRouter basename="/dms">
        <Routes>
          {/* Auth and Navigation Routes */}
          <Route path="/home/login" element={<Login />} />
          <Route path="/home/register" element={<Register />} />
          <Route path="/home/forgot" element={<ForgotPassword />} />
          <Route path="/home/reset" element={<ResetPassword />} />

          {/* Authenticated Routes */}
          <Route path="/home" element={<Home />} />
          <Route path="/home/dashboard" element={<Dashboard />} />
          <Route path="/home/userdashboard" element={<UserDashboard />} />
          <Route path="/home/search" element={<Search />} />
          <Route path="/home/editprofile" element={<EditProfile />} />
          <Route path="/home/managecauselist" element={<ManageCauseList />} />
          <Route path="/pdf" element={<PDFViewer />} />
        </Routes>
      </BrowserRouter>
    </div>
  );
}
