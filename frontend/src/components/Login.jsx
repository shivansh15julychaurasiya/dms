import React, { useState } from "react";

import { Link, useNavigate } from "react-router-dom";
import { loginUser } from "../services/axios"; // Your custom axios function for login

const Login = () => {
  const [formData, setFormData] = useState({ loginId: "", password: "" });
  const [errorMsg, setErrorMsg] = useState("");
  const navigate = useNavigate(); // Correctly using useNavigate hook

  // Function to handle form input changes
  const handleChange = (e) => {
    setFormData((prev) => ({
      ...prev,
      [e.target.name]: e.target.value,
    }));
  };

  // Function to handle login submission
  const handleLogin = async (e) => {
    e.preventDefault();
    setErrorMsg(""); // Reset error message before making API request

    try {
      // Attempt to log the user in using the loginUser function
      const { data } = await loginUser(formData.loginId, formData.password, setErrorMsg, navigate);

      // Assuming the response contains token and user data
      localStorage.setItem("token", data.token);
      localStorage.setItem("user", JSON.stringify(data.user));

    } catch (err) {

      console.log(err.message);
      // setErrorMsg("Invalid credentials. Please try again.");
    }
  };

  // Styles for the login page
  const wrapperStyle = {
    height: "100vh",
    backgroundImage:
      "url('https://as2.ftcdn.net/jpg/06/12/69/39/1000_F_612693965_Ic0XfvkMa44xQXHA8lonULgqoEzyS0Xl.jpg')",
    backgroundSize: "cover",
    backgroundPosition: "center",
    display: "flex",
    alignItems: "center",
    justifyContent: "center",
  };

  const cardStyle = {
    backgroundColor: "rgba(255, 255, 255, 0.95)",
    padding: "2rem",
    borderRadius: "2rem",
    maxWidth: "410px",
    width: "100%",
    boxShadow: "0 4px 20px rgba(0, 0, 0, 0.3)",
  };

  return (
    <div style={wrapperStyle}>
      <div style={cardStyle}>
        <h2 className="text-center mb-4 fw-bold shimmer-text">
          <i className="bi bi-person-circle me-2"></i>Login
        </h2>

        {errorMsg && (
          <div className="alert alert-danger py-1 text-center">{errorMsg}</div>
        )}

        <form onSubmit={handleLogin}>
          <div className="mb-3">
            <label htmlFor="loginId" className="form-label fw-bold shimmer-text">
              User ID:
            </label>
            <input
              type="number"
              className="form-control"
              id="loginId"
              placeholder="Enter User ID"
              name="loginId"
              value={formData.loginId}
              onChange={handleChange}
              required
            />
          </div>

          <div className="mb-3">
            <label htmlFor="password" className="form-label fw-bold shimmer-text">
              Password:
            </label>
            <input
              type="password"
              className="form-control"
              id="password"
              placeholder="Enter password"
              name="password"
              value={formData.password}
              onChange={handleChange}
              required
            />
          </div>

          <div className="d-grid">
            <button type="submit" className="btn btn-primary login-btn">
              <i className="bi bi-box-arrow-in-right me-1"></i> Login
            </button>
          </div>

          <div className="text-center mt-3">
            <Link to="/home/forgot" className="text-danger fw-bold shimmer-text">
              Forgot Password?
            </Link>
          </div>
        </form>
      </div>
    </div>
  );
};

export default Login;
