// ProfileCard.js
import React, { useEffect, useState } from "react";
import { Link } from "react-router-dom"; // Import Link from react-router-dom

const ProfileCard = () => {
  const [user, setUser] = useState(null);

  
  // Fetch user data from localStorage on mount
  useEffect(() => {
    const storedUser = localStorage.getItem("user");
    const token = localStorage.getItem("token");

    if (storedUser && token) {
      setUser(JSON.parse(storedUser)); // Parse the stored user data from localStorage
    } else {
      setUser(null); // If no user data or token, set user to null
    }
  }, []);


  
  const handleLogout = async () => {
    const token = localStorage.getItem("token");
  
    try {
      const res = await fetch("http://localhost:8081/dms/auth/logout", {
        method: "GET",
        headers: {
          Authorization: `Bearer ${token}`,
        },
      });
  
      if (!res.ok) throw new Error("Token expired or unauthorized");
  
      const data = await res.json();
      console.log("Logout success:", data.message);
    } catch (error) {
      console.warn("Logout failed:", error.message);
    } finally {
      // Remove token from localStorage
      localStorage.removeItem("token");
      
      // Trigger logout across all tabs by setting a value in localStorage
      localStorage.setItem("logout", Date.now().toString());
      window.location.href = "/dms/home/login"; // Redirect to login page
    }
  };
  

  const handleProfile = () => {
    window.location.href = "/dms/home/editprofile";
  };

  return (
    <div className="container d-flex justify-content-center mt-5">
      <div className="card p-4 shadow-lg rounded-4" style={{ maxWidth: "400px" }}>
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
              // Replace <a> with <Link> for client-side navigation
              <Link to="/dms/home/login" className="btn btn-outline-primary rounded-pill px-4">
                Login
              </Link>
            )}
          </div>
        </div>
      </div>
    </div>
  );
};

export default ProfileCard;
