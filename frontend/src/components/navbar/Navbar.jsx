import React, { useEffect, useState } from "react";
import { FaUserCircle } from "react-icons/fa";
import "bootstrap/dist/css/bootstrap.min.css";
import "bootstrap-icons/font/bootstrap-icons.css";
import "bootstrap/dist/js/bootstrap.bundle.min.js";
import ProfileCard from "../ProfileCard";

import "../../assets/styles.css";
 // ✅ this should work if styles.css is inside src/assets/




const Navbar = () => {
  const [currentDateTime, setCurrentDateTime] = useState(new Date());

  useEffect(() => {
    const interval = setInterval(() => {
      setCurrentDateTime(new Date());
    }, 1000);
    return () => clearInterval(interval);
  }, []);

  const formattedDateTime = currentDateTime.toLocaleString("en-US", {
    weekday: "long",
    year: "numeric",
    month: "long",
    day: "2-digit",
    hour: "2-digit",
    minute: "2-digit",
    second: "2-digit",
    hour12: true,
  });

  return (
    <>
      <nav
        className="navbar bg-light shadow-sm fixed-top"
        style={{
          height: "50px",
          borderBottom: "0.5px solid rgb(231, 228, 228)",
          fontSize: "14px",
          color: "#555",
          zIndex: 1040,
        }}
      >
        <div className="container-fluid d-flex justify-content-between align-items-center px-4">
          <a className="navbar-brand fw-bold px-5 shimmer-text" href="#">
            High Court
          </a>

          <div className="d-flex align-items-center gap-3">
            <div className="d-none d-md-inline text-muted small">{formattedDateTime}</div>

            <button
              className="btn btn-link text-decoration-none"
              data-bs-toggle="modal"
              data-bs-target="#profileModal"
              title="View Profile"
            >
              <FaUserCircle size={26} className="text-primary" />
            </button>

            <span className="fw-semibold d-none d-md-inline  shimmer-text">E-High Court</span>
          </div>
        </div>
      </nav>

      {/* Spacer */}
      <div style={{ height: "50px" }}></div>

      {/* Profile Modal */}
      <div
        className="modal fade"
        id="profileModal"
        tabIndex="-1"
        aria-labelledby="profileModalLabel"
        aria-hidden="true"
      >
        <div className="modal-dialog custom-top-right-modal">
          <div className="modal-content border-0 shadow">
            <div className="modal-header border-0">
              <h5 className="modal-title fw-bold" id="profileModalLabel">
                User Profile
              </h5>
              <button
                type="button"
                className="btn-close"
                data-bs-dismiss="modal"
                aria-label="Close"
              ></button>
            </div>
            <div className="modal-body">
              <ProfileCard />
            </div>
          </div>
        </div>
      </div>

     
    </>
  );
};

export default Navbar;
