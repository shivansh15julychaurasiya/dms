import React from "react";
import { BrowserRouter as Router, Routes, Route } from "react-router-dom";

import Login from "./Pages/Login/Login";
import Dashboard from "./Pages/Dashboard/Dashboard";
import NotFound from "./Pages/NotFound/NotFound";
import PrivateRoute from "./component/ProtectedRoute";
import UserManagement from "./component/UserManagement";
import CategoryManagement from "./Pages/Category/CategoryManagement";
import SubCategoryManagement from "./Pages/SubCategory/SubCategoryManagement";
import ProductManagement from "./Pages/Products/ProductManagement";

import { AuthProvider } from "./context/AuthContext" //  added

export default function App() {
  return (
    <AuthProvider>   {/* Wrap entire app */}
      <Router>
        <Routes>
          {/* Login */}
          <Route path="/" element={<Login />} />

          {/* Protected Dashboard */}
          <Route
            path="/dashboard"
            element={
              <PrivateRoute>
                <Dashboard />
              </PrivateRoute>
            }
          />

          {/* Protected User Management */}
          <Route
            path="/users"
            element={
              <PrivateRoute>
                <UserManagement />
              </PrivateRoute>
            }
          />
          <Route
            path="/products"
            element={
              <PrivateRoute>
                <ProductManagement />
              </PrivateRoute>
            }
          />
          <Route
            path="/categories"
            element={
              <PrivateRoute>
                <CategoryManagement />
              </PrivateRoute>
            }
          />
          <Route
            path="/subcategories"
            element={
              <PrivateRoute>
                <SubCategoryManagement />
              </PrivateRoute>
            }
          />            

          {/* 404 Page */}
          <Route path="*" element={<NotFound />} />
        </Routes>
      </Router>
    </AuthProvider>
  );
}
