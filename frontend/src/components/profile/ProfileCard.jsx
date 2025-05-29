import React, { useEffect } from "react";
import { FaTimes } from "react-icons/fa";
import { Button } from "reactstrap";
import { useAuth } from "../../context/AuthContext";
import { isTokenExpired, showAlert } from "../../utils/helpers";

const ProfileCard = ({ toggleModal }) => {
  const { user, token, logout } = useAuth();

  useEffect(() => {
    if (!user || !token || isTokenExpired(token)) {
      logout();
    }
  }, [user, token, logout]);

  const handleLogout = () => {
    logout();
    showAlert("You are logged out!", "success");
  };

  // const handleProfile = () => {
  //   window.location.href = "/dms/home/editprofile";
  // };

  const handleChangePassword = () => {
    window.location.href = "/dms/home/changepassword";
  };

  const manageLogin = () => {
    window.location.href = "/dms/home/login";
  };

  return (
    <div className="container d-flex justify-content-center p-0">
      <div className="card shadow rounded-4 p-4 position-relative" style={{ maxWidth: "330px", width: "90%",height:"50%" }}>
        {/* Close Button */}
        <Button
          color="link"
          onClick={toggleModal}
          className="position-absolute top-0 end-0 mt-1 me-2 p-1"
          style={{ borderRadius: "50%" }}
          title="Close"
        >
          <FaTimes size={16} className="text-danger" />
        </Button>

        <div className="d-flex flex-column align-items-center text-center">
          <img
            src="https://cdn.pixabay.com/photo/2022/09/08/15/16/cute-7441224_1280.jpg"
            alt="Profile"
            className="rounded-circle mb-1 border"
            width="65"
            height="65"
          />
          <h6 className="fw-bold mb-1">{user ? user.name : "Guest"}</h6>
          <p className="text-muted mb-1 small">{user ? user.email : "Login to view details"}</p>
          <p className="text-muted small mb-2">{user ? user.about : "You are viewing the profile as a guest."}</p>

          {/* Small Button Group */}
          <div className="d-flex flex-column gap-1 w-100">
            {user ? (
              <>
              {/*  <button className="btn btn-primary btn-sm rounded-pill px-2 py-1" onClick={handleProfile}>
                  <i className="bi bi-pencil me-1"></i>Edit
                </button>
                */}
                <button className="btn btn-warning btn-sm rounded-pill px-1 mt-1 py-1" onClick={handleChangePassword}>
                  <i className="bi bi-key me-1"></i>Reset-password
                </button>
                <button className="btn btn-danger btn-sm rounded-pill px-2 py-1 mt-2" onClick={handleLogout}>
                  <i className="bi bi-box-arrow-right me-1"></i>Logout
                </button>
              </>
            ) : (
              <button className="btn btn-success btn-sm rounded-pill px-3 py-1" onClick={manageLogin}>
                <i className="bi bi-box-arrow-in-right me-1"></i>Login
              </button>
            )}
          </div>
        </div>
      </div>
    </div>
  );
};

export default ProfileCard;
