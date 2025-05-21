import React, { useEffect } from "react";
import { BrowserRouter, Routes, Route } from "react-router-dom";
import "bootstrap/dist/css/bootstrap.min.css";
import "bootstrap-icons/font/bootstrap-icons.css";
import { ToastContainer } from "react-toastify";
import "react-toastify/dist/ReactToastify.css";
import Home from "./pages/Home";
import Login from "./components/auth/Login";
import Register from "./components/auth/Register";
import ForgotPassword from "./components/auth/ForgotPassword";
import ResetPassword from "./components/auth/ResetPassword";
import Dashboard from "./components/dashboard/DashboardLayout";
import AdminDashboard from "./components/dashboard/AdminDashboard";
import UserDashboard from "./components/dashboard/UserDashboard";
import Search from "./pages/Search";
import EditProfile from "./components/profile/EditProfile";
import ManageCauseList from "./pages/ManageCauseList";
import PDFViewer from "./components/pdf/PDFViewer";
import UnAuthorize from "./pages/UnAuthorize";
import { AuthProvider } from "./context/AuthContext";
import ProtectedRoute from "./components/auth/ProtectedRoute";
import UpdateUser from "./components/auth/UpdateUser";
import ReservedCases from "./pages/ReservedCases";
import ChangePassword from "./components/auth/ChangePassword";

export default function App() {
  useEffect(() => {
    const handleStorageChange = (event) => {
      if (event.key === "logout") {
        window.location.href = "/dms/";
      }
    };
    window.addEventListener("storage", handleStorageChange);
    return () => window.removeEventListener("storage", handleStorageChange);
  }, []);

  return (
    <AuthProvider>
      <ToastContainer
        position="top-center" // Position on screen
        autoClose={500} // Auto close after 3 seconds
        hideProgressBar={false} // Show the progress bar
        newestOnTop={false} // Newest toasts appear on top
        closeOnClick // Close on click
        rtl={false} // Left-to-right layout
        pauseOnFocusLoss // Pause when tab loses focus
        draggable // Allow drag to dismiss
        pauseOnHover // Pause on hover
        theme="colored" // "light" | "dark" | "colored"
      />

      <BrowserRouter basename="/dms">
        <Routes>
          <Route path="" element={<Login />} />
          <Route path="/home/register" element={<Register />} />

          <Route path="/home/forgot" element={<ForgotPassword />} />
          <Route path="/home/reset" element={<ResetPassword />} />
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
            path="/home/changepassword"
            element={
              <ProtectedRoute>
                <ChangePassword />
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
            path="/home/reservedcases"
            element={
              <ProtectedRoute>
                <ReservedCases/>
              </ProtectedRoute>
            }
          />
          <Route
            path="/home/updateuser/:userId"
            element={
              <ProtectedRoute>
                <UpdateUser />
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
          {/* <Route path="/home/unauthorize" element={<UnAuthorize />} /> */}
        </Routes>
      </BrowserRouter>
    </AuthProvider>
  );
}
