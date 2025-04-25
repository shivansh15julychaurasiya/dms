import React, { useEffect } from "react";
import { useAuth } from "../../context/AuthContext";
import { isTokenExpired } from "../../utils/helpers";

const ProfileCard = () => {
  const { user, token, logout } = useAuth();

  // Auto logout if user/token is missing or token is expired
  useEffect(() => {
    if (!user || !token || isTokenExpired(token)) {
      logout();
      // window.location.href = "/dms/home/login"; // Force redirect to login
    }
  }, [user, token, logout]);

  const handleLogout = async () => {
    logout();
  };

  const handleProfile = () => {
    window.location.href = "/dms/home/editprofile";
  };

  const manageLogin = () => {
    window.location.href = "/dms/home/login";
  };

  return (
    <div className="container d-flex justify-content-center mt-5">
      <div
        className="card p-4 shadow-lg rounded-4"
        style={{ maxWidth: "400px" }}
      >
        <div className="d-flex flex-column align-items-center text-center">
          <img
            src="https://cdn.pixabay.com/photo/2022/09/08/15/16/cute-7441224_1280.jpg"
            alt="Profile"
            className="rounded-circle"
            width="100"
            height="100"
          />
          <h4 className="mb-0">{user ? user.name : "Guest"}</h4>
          <small className="text-muted mb-3">
            {user ? user.email : "Please login to view your details"}
          </small>
          <p className="text-center text-muted">
            {user ? user.about : "You are viewing the profile as a guest."}
          </p>

          <div className="d-flex gap-2">
            {user ? (
              <>
                <button
                  className="btn btn-primary px-4 rounded-pill"
                  onClick={handleLogout}
                >
                  <i className="bi bi-box-arrow-right p-2 btn-sm"></i>
                  Log-out
                </button>

                <button
                  className="btn btn-success btn-sm px-4 rounded-pill"
                  onClick={handleProfile}
                >
                  <i className="bi bi-pencil p-2"></i>
                  Edit Profile
                </button>
              </>
            ) : (
              <button
                className="btn btn-outline-primary rounded-pill px-4"
                onClick={manageLogin}
              >
                Login
              </button>
            )}
          </div>
        </div>
      </div>
    </div>
  );
};

export default ProfileCard;
