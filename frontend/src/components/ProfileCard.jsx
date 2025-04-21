import React from 'react';


const ProfileCard = () => {



  const handleLogout = async () => {
    const token = localStorage.getItem("token");
    
    

    try {
      const res = await fetch("http://localhost:8081/dms/auth/logout", {
        method: "GET",
        headers: {
          Authorization: `Bearer ${token}`,
        },
      });

      // if (!res.ok) throw new Error("Logout failed");

      const data = await res.json();
      console.log("Logout success:", data.message);

      // Clear local storage and redirect
      localStorage.removeItem("token");

      window.location.href = "/home/login";
    } catch (error) {
      console.error("Logout error:", error.message);
      alert("Error logging out");
    }
  };


  const handleProfile=()=>{
    window.location.href = "/home/editprofile";

  }

  return (
    <div className="container d-flex justify-content-center mt-5">
      <div className="card p-4 shadow-lg rounded-4" style={{ maxWidth: '400px' }}>
        <div className="d-flex flex-column align-items-center text-center">
          <img
            src="https://cdn.pixabay.com/photo/2022/09/08/15/16/cute-7441224_1280.jpg"
            alt="Profile"
            className="rounded-circle"
            width="100"
            height="100"
          />
          <h4 className="mb-0">Vijay Chaurasiya</h4>
          <small className="text-muted mb-3">Java FullStack Developer</small>

          <p className="text-center text-muted">
            Passionate developer with a love for building beautiful and functional user interfaces.
          </p>

          <div className="d-flex gap-2">
            <button
              className="btn btn-primary px-4 rounded-pill"
              onClick={handleLogout}
            >
              <i className="bi bi-box-arrow-right p-2 btn-sm"></i>
              Log-out
            </button>

            <button className="btn btn-success btn-sm px-4 rounded-pill" onClick={handleProfile}>
              <i className="bi bi-pencil p-2"></i>
              Edit Profile
            </button>
          </div>
        </div>
      </div>
    </div>
  );
};

export default ProfileCard;
