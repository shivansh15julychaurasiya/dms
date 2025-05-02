import React, { useState, useEffect } from "react";
import { Link, useLocation } from "react-router-dom";
import {
  FaFileAlt,
  FaCalendarAlt,
  FaChartBar,
  FaBars,
} from "react-icons/fa";
import "../../assets/styles.css";
import { useAuth } from "../../context/AuthContext";

const Sidebar = () => {
  const [isOpen, setIsOpen] = useState(true);
  const [activeDropdown, setActiveDropdown] = useState("");
  const { role } = useAuth();
  const location = useLocation();

  const toggleSidebar = () => setIsOpen((prev) => !prev);

  const toggleDropdown = (item) =>
    setActiveDropdown((prev) => (prev === item ? "" : item));

  const isActive = (path) => location.pathname === path;

  // Automatically open dropdown if URL matches any submenu
  useEffect(() => {
    if (
      location.pathname.startsWith("/home/") ||
      location.pathname.startsWith("/cases_mgmt")
    ) {
      setActiveDropdown("Home");
    } else if (location.pathname.startsWith("/scheduling")) {
      setActiveDropdown("Scheduling");
    }
  }, [location.pathname]);

  return (
    <>
      <div
        className={`position-fixed top-0 start-0 h-100 bg-dark border-end shadow-sm ${
          isOpen ? "px-3" : "px-2"
        }`}
        style={{
          width: isOpen ? "250px" : "70px",
          transition: "all 0.3s ease",
          zIndex: 1000,
        }}
      >
        <div className="text-center text-light py-3 border-bottom">
          {isOpen && <h5 className="text-primary m-0"></h5>}
        </div>

        <ul className="nav flex-column mt-4">
          {/* Home Dropdown */}
          <li className="nav-item">
            <div className="nav-link d-flex justify-content-between fw-bold align-items-center text-light hover-effect sidebar-hover">
              <div
                onClick={() => toggleDropdown("Home")}
                style={{ cursor: "pointer", flex: 1 }}
                className="d-flex justify-content-between align-items-center w-100"
              >
                <span>
                  <FaFileAlt className="me-2" />
                  {isOpen && "Home"}
                </span>
                {isOpen && (
                  <span>{activeDropdown === "Home" ? "▲" : "▼"}</span>
                )}
              </div>
            </div>
            {activeDropdown === "Home" && isOpen && (
              <ul className="nav flex-column ms-3">
                <li className="nav-item">
                  <Link
                    to="/home/dashboard"
                    className={`nav-link px-2 text-light ${
                      isActive("/home/dashboard") ? "bg-primary rounded" : ""
                    }`}
                  >
                    Dashboard
                  </Link>
                </li>
                <li className="nav-item">
                  <Link
                    to="/home/search"
                    className={`nav-link px-2 text-light ${
                      isActive("/home/search") ? "bg-primary rounded" : ""
                    }`}
                  >
                    Search
                  </Link>
                </li>
                {role === "ROLE_ADMIN" && (
                  <li className="nav-item">
                    <Link
                      to="/home/admindashboard"
                      className={`nav-link px-2 text-light ${
                        isActive("/home/admindashboard") ? "bg-primary rounded" : ""
                      }`}
                    >
                      Manage Users
                    </Link>
                  </li>
                )}
                <li className="nav-item">
                  <Link
                    to="/home/managecauselist"
                    className={`nav-link px-2 text-light ${
                      isActive("/home/managecauselist") ? "bg-primary rounded" : ""
                    }`}
                  >
                    Manage CauseList
                  </Link>
                </li>
                <li className="nav-item">
                  <Link
                    to="/home/reservedcases"
                    className={`nav-link px-2 text-light ${
                      isActive("/home/reservedcases") ? "bg-primary rounded" : ""
                    }`}
                  >
                    Reserved Cases
                  </Link>
                </li>
                <li className="nav-item">
                  <Link
                    to="/cases_mgmt/status"
                    className={`nav-link px-2 text-light ${
                      isActive("/cases_mgmt/status") ? "bg-primary rounded" : ""
                    }`}
                  >
                    Status
                  </Link>
                </li>
              </ul>
            )}
          </li>

          {/* Analytics */}
          <li className="nav-item">
            <Link
              to="/analytics"
              className={`nav-link text-light fw-bold d-flex align-items-center hover-effect sidebar-hover ${
                isActive("/analytics") ? "bg-primary rounded" : ""
              }`}
            >
              <FaChartBar className="me-2" />
              {isOpen && "Analytics"}
            </Link>
          </li>

          {/* Scheduling Dropdown */}
          <li className="nav-item">
            <div className="nav-link d-flex justify-content-between fw-bold align-items-center text-light hover-effect sidebar-hover">
              <div
                onClick={() => toggleDropdown("Scheduling")}
                style={{ cursor: "pointer", flex: 1 }}
                className="d-flex justify-content-between align-items-center w-100"
              >
                <span>
                  <FaCalendarAlt className="me-2" />
                  {isOpen && "Scheduling"}
                </span>
                {isOpen && (
                  <span>{activeDropdown === "Scheduling" ? "▲" : "▼"}</span>
                )}
              </div>
            </div>
            {activeDropdown === "Scheduling" && isOpen && (
              <ul className="nav flex-column ms-3">
                <li className="nav-item">
                  <Link
                    to="/scheduling/court-calendar"
                    className={`nav-link px-2 text-light ${
                      isActive("/scheduling/court-calendar") ? "bg-primary rounded" : ""
                    }`}
                  >
                    Court Calendar
                  </Link>
                </li>
                <li className="nav-item">
                  <Link
                    to="/scheduling/hearing-schedule"
                    className={`nav-link px-2 text-light ${
                      isActive("/scheduling/hearing-schedule") ? "bg-primary rounded" : ""
                    }`}
                  >
                    Hearing Schedule
                  </Link>
                </li>
                <li className="nav-item">
                  <Link
                    to="/scheduling/set-appointment"
                    className={`nav-link px-2 text-light ${
                      isActive("/scheduling/set-appointment") ? "bg-primary rounded" : ""
                    }`}
                  >
                    Set Appointment
                  </Link>
                </li>
              </ul>
            )}
          </li>
        </ul>
      </div>

      {/* Sidebar Toggle Button */}
      <button
        className="btn px-3 btn-lg position-fixed top-0 start-0 m-1"
        onClick={toggleSidebar}
        style={{ zIndex: 1100 }}
      >
        <FaBars />
      </button>

      {/* Content Spacer */}
      <div
        style={{
          marginLeft: isOpen ? "250px" : "70px",
          transition: "margin-left 0.3s ease",
        }}
      ></div>
    </>
  );
};

export default Sidebar;
