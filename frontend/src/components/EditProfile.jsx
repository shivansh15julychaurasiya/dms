import React from "react";
import "bootstrap/dist/css/bootstrap.min.css";
import "bootstrap-icons/font/bootstrap-icons.css";
import { Link } from "react-router-dom";

const EditProfile = () => {
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
    maxWidth: "500px",
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
        <h2 className="text-center mb-4">
          <i className="bi bi-pencil-square me-2"></i>Edit Profile
        </h2>
        <form>
          <div className="mb-3">
            <label htmlFor="name" className="form-label">
              Full Name
            </label>
            <input
              type="text"
              className="form-control"
              id="name"
              placeholder="Enter full name"
            />
          </div>
          <div className="mb-3">
            <label htmlFor="email" className="form-label">
              Email Address
            </label>
            <input
              type="email"
              className="form-control"
              id="email"
              placeholder="Enter email"
            />
          </div>
          <div className="mb-3">
            <label htmlFor="password" className="form-label">
              New Password
            </label>
            <input
              type="password"
              className="form-control"
              id="password"
              placeholder="Enter new password"
            />
          </div>
          <div className="d-grid">
            <button
              type="submit"
              className="btn btn-primary"
              style={btnStyle}
              onMouseOver={(e) =>
                (e.target.style.backgroundColor = btnHoverStyle.backgroundColor)
              }
              onMouseOut={(e) =>
                (e.target.style.backgroundColor = btnStyle.backgroundColor)
              }
            >
              <i className="bi bi-save me-1"></i> Save Changes
            </button>
          </div>
          <div className="text-center mt-3">
            <Link to="/home" className="text-decoration-none">
              <i className="bi bi-arrow-left-circle me-1"></i>Back to Home
            </Link>
          </div>
        </form>
      </div>
    </div>
  );
};

export default EditProfile;
