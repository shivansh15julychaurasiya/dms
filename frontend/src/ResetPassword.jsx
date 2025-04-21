import React, { useState } from "react";
import axios from "axios";
import "bootstrap/dist/css/bootstrap.min.css";
import "bootstrap-icons/font/bootstrap-icons.css";
import { Link, useNavigate } from "react-router-dom";

const ResetPassword = () => {
  const [loginId, setLoginId] = useState("");
  const [newPassword, setNewPassword] = useState("");
  const [message, setMessage] = useState("");
  const navigate = useNavigate();

  const handleResetPassword = async (e) => {
    e.preventDefault();

    const token = localStorage.getItem("token"); // Get token from storage

    if (!token) {
      setMessage("Reset token not found. Please verify OTP again.");
      return;
    }

    try {
      const response = await axios.post(
        "http://localhost:8081/dms/auth/reset-password",
        {
          login_id: loginId,
          password: newPassword,
        },
        {
          headers: {
            Authorization: `Bearer ${token}`,
          },
        }
      );

      setMessage("Password has been reset successfully!");
      setTimeout(() => navigate("/home/login"), 1000); // Redirect after 2s
    } catch (error) {
      console.error("Reset failed: ", error);
      setMessage("Failed to reset password. Please check your details.");
    }
  };

  return (
    <div
      style={{
        height: "100vh",
        backgroundImage:
          "url('https://as2.ftcdn.net/jpg/06/12/69/39/1000_F_612693965_Ic0XfvkMa44xQXHA8lonULgqoEzyS0Xl.jpg')",
        backgroundSize: "cover",
        backgroundPosition: "center",
        display: "flex",
        alignItems: "center",
        justifyContent: "center",
      }}
    >
      <div
        style={{
          backgroundColor: "rgba(255, 255, 255, 0.95)",
          padding: "2rem",
          borderRadius: "1.5rem",
          maxWidth: "400px",
          width: "100%",
          boxShadow: "0 4px 20px rgba(0, 0, 0, 0.3)",
        }}
      >
        <h2 className="text-center mb-4">
          <i className="bi bi-key-fill me-2"></i>Reset Password
        </h2>
        <p className="text-muted text-center mb-4">
          Enter your Login ID and new password to reset your account.
        </p>

        <form onSubmit={handleResetPassword}>
          <div className="mb-3">
            <label htmlFor="loginId" className="form-label">
              Login ID:
            </label>
            <input
              type="text"
              className="form-control"
              id="loginId"
              value={loginId}
              onChange={(e) => setLoginId(e.target.value)}
              placeholder="Enter Login ID"
              required
            />
          </div>

          <div className="mb-3">
            <label htmlFor="newPassword" className="form-label">
              New Password:
            </label>
            <input
              type="password"
              className="form-control"
              id="newPassword"
              value={newPassword}
              onChange={(e) => setNewPassword(e.target.value)}
              placeholder="Enter New Password"
              required
            />
          </div>

          <div className="d-grid">
            <button
              type="submit"
              className="btn btn-danger"
              onMouseOver={(e) => (e.target.style.backgroundColor = "#e64949")}
              onMouseOut={(e) => (e.target.style.backgroundColor = "#ff6b6b")}
              style={{ backgroundColor: "#ff6b6b", border: "none" }}
            >
              <i className="bi bi-shield-lock-fill me-1"></i> Reset Password
            </button>
          </div>
        </form>

        {message && (
          <div className="alert alert-info mt-3 text-center" role="alert">
            {message}
          </div>
        )}

        <div className="text-center mt-3">
          <Link to="/home/login" className="text-decoration-none">
            <i className="bi bi-arrow-left me-1"></i> Back to Login
          </Link>
        </div>
      </div>
    </div>
  );
};

export default ResetPassword;
