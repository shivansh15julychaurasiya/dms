import React, { useState } from "react";
import axios from "axios";
import "bootstrap/dist/css/bootstrap.min.css";
import "bootstrap-icons/font/bootstrap-icons.css";
import { Link, useNavigate } from "react-router-dom";

const Register = () => {
  const navigate = useNavigate();

  const [formData, setFormData] = useState({
    name: "",
    email: "",
    about: "",
    password: "",
  });

  const [loading, setLoading] = useState(false);

  const handleChange = (e) => {
    setFormData({ ...formData, [e.target.id]: e.target.value });
  };

  const handleSubmit = async (e) => {
    e.preventDefault();
    setLoading(true);
    try {
      const response = await axios.post("http://localhost:8081/dms/auth/register", formData);
      console.log("Registration successful:", response.data);
      alert("Registration successful! Please login.");
      navigate("/home/login");
    } catch (error) {
      console.error("Error during registration:", error.response?.data || error.message);
      alert("Registration failed. Please try again.");
    } finally {
      setLoading(false);
    }
  };

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
    borderRadius: "1.5rem",
    maxWidth: "450px",
    width: "100%",
    boxShadow: "0 4px 20px rgba(0, 0, 0, 0.3)",
  };

  const btnStyle = {
    backgroundColor: "#6c63ff",
    border: "none",
  };

  const btnHoverStyle = {
    backgroundColor: "#574fd6",
  };

  return (
    <div style={wrapperStyle}>
      <div style={cardStyle}>
        <h2 className="text-center mb-2">
          <i className="bi bi-person-plus-fill me-2"></i>Register
        </h2>
        <form onSubmit={handleSubmit}>
          <div className="mb-3">
            <label htmlFor="name" className="form-label">Full Name</label>
            <input
              type="text"
              className="form-control"
              id="name"
              placeholder="Enter your name"
              value={formData.name}
              onChange={handleChange}
              required
            />
          </div>
          <div className="mb-3">
            <label htmlFor="email" className="form-label">Email Address</label>
            <input
              type="email"
              className="form-control"
              id="email"
              placeholder="Enter email"
              value={formData.email}
              onChange={handleChange}
              required
            />
          </div>
          <div className="mb-3">
            <label htmlFor="about" className="form-label">About</label>
            <textarea
              className="form-control"
              id="about"
              rows="3"
              placeholder="Tell us about yourself"
              value={formData.about}
              onChange={handleChange}
            ></textarea>
          </div>
          <div className="mb-3">
            <label htmlFor="password" className="form-label">Password</label>
            <input
              type="password"
              className="form-control"
              id="password"
              placeholder="Enter password"
              value={formData.password}
              onChange={handleChange}
              required
            />
          </div>
          <div className="d-grid">
            <button
              type="submit"
              className="btn btn-primary"
              style={btnStyle}
              onMouseOver={(e) => (e.target.style.backgroundColor = btnHoverStyle.backgroundColor)}
              onMouseOut={(e) => (e.target.style.backgroundColor = btnStyle.backgroundColor)}
              disabled={loading}
            >
              {loading ? "Registering..." : <><i className="bi bi-person-check-fill me-1"></i> Register</>}
            </button>
          </div>
          <div className="text-center mt-3 text-dark">
            Already have an account?{" "}
            <Link to="/home/login" className="text-decoration-none fw-bold">
              Login here
            </Link>
          </div>
        </form>
      </div>
    </div>
  );
};

export default Register;
