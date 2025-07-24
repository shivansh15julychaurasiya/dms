import React, { useState, useEffect } from "react";
import { Link, useLocation } from "react-router-dom";
import { FaFileAlt, FaCalendarAlt, FaChartBar, FaBars } from "react-icons/fa";
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

  useEffect(() => {
    if (
      location.pathname.startsWith("/home/") ||
      location.pathname.startsWith("/cases_mgmt")
    ) {
      setActiveDropdown("Home");
    } else if (location.pathname.startsWith("/casefile")) {
      setActiveDropdown("Transaction");
    }
  }, [location.pathname]);

  const sidebarHoverStyle = (
    <style>
      {`
        .sidebar-hover {
          position: relative;
          transition: background-color 0.3s ease;
        }

        .sidebar-hover:hover {
          background-color: rgb(126, 61, 21) !important;
          color: #fff !important;
          border-radius: 5px;
        }

        .sidebar-hover:hover svg,
        .sidebar-hover:hover span {
          color: #fff !important;
        }

        .custom-active-link {
          background-color: rgb(96, 22, 194) !important;
          color: #fff !important;
          border-radius: 5px;
          position: relative;
        }

        .custom-active-link::before,
        .sidebar-hover:hover::before {
          content: '';
          position: absolute;
          right: 0;
          top: 10%;
          bottom: 10%;
          width: 4px;
          background-color: orange;
          border-radius: 3px;
          transition: all 0.3s ease-in-out;
        }

        .custom-active-link.sidebar-hover:hover::before {
          background-color: #fff;
        }
      `}
    </style>
  );

  return (
    <>
      {sidebarHoverStyle}

      <div
        className={`position-fixed top-0 start-0 h-100 bg-light border-end shadow-sm ${
          isOpen ? "px-3" : "px-2"
        }`}
        style={{
          width: isOpen ? "250px" : "70px",
          transition: "all 0.3s ease",
          zIndex: 1000,
        }}
      >
        <div className="text-center text-dark py-3 border-bottom">
          {isOpen && <h5 className="text-primary m-1"></h5>}
        </div>

        <ul className="nav flex-column mt-4">
          {/* Home Dropdown */}
          <li className="nav-item">
            <div className="nav-link d-flex justify-content-between fw-bold align-items-center text-dark sidebar-hover">
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
                    className={`nav-link px-2 text-dark sidebar-hover ${
                      isActive("/home/dashboard")
                        ? "custom-active-link"
                        : ""
                    }`}
                  >
                    Dashboard
                  </Link>
                </li>
                <li className="nav-item">
                  <Link
                    to="/home/search"
                    className={`nav-link px-2 text-dark sidebar-hover ${
                      isActive("/home/search")
                        ? "custom-active-link"
                        : ""
                    }`}
                  >
                    Search
                  </Link>
                </li>
                {role === "ROLE_ADMIN" && (
                  <li className="nav-item">
                    <Link
                      to="/home/admindashboard"
                      className={`nav-link px-2 text-dark sidebar-hover ${
                        isActive("/home/admindashboard")
                          ? "custom-active-link"
                          : ""
                      }`}
                    >
                      Manage Users
                    </Link>
                  </li>
                )}
                <li className="nav-item">
                  <Link
                    to="/home/managecauselist"
                    className={`nav-link px-2 text-dark sidebar-hover ${
                      isActive("/home/managecauselist")
                        ? "custom-active-link"
                        : ""
                    }`}
                  >
                    Manage CauseList
                  </Link>
                </li>
                <li className="nav-item">
                  <Link
                    to="/home/reservedcases"
                    className={`nav-link px-2 text-dark sidebar-hover ${
                      isActive("/home/reservedcases")
                        ? "custom-active-link"
                        : ""
                    }`}
                  >
                    Reserved Cases
                  </Link>
                </li>
                <li className="nav-item">
                  <Link
                    to="/cases_mgmt/status"
                    className={`nav-link px-2 text-dark sidebar-hover ${
                      isActive("/cases_mgmt/status")
                        ? "custom-active-link"
                        : ""
                    }`}
                  >
                    Status
                  </Link>
                </li>
              </ul>
            )}
          </li>

          {/* Transaction Dropdown */}
          <li className="nav-item">
            <div className="nav-link d-flex justify-content-between fw-bold align-items-center text-dark sidebar-hover">
              <div
                onClick={() => toggleDropdown("Transaction")}
                style={{ cursor: "pointer", flex: 1 }}
                className="d-flex justify-content-between align-items-center w-100"
              >
                <span>
                  <FaFileAlt className="me-2" />
                  {isOpen && "Transaction"}
                </span>
                {isOpen && (
                  <span>{activeDropdown === "Transaction" ? "▲" : "▼"}</span>
                )}
              </div>
            </div>
            {activeDropdown === "Transaction" && isOpen && (
              <ul className="nav flex-column ms-3">
                <li className="nav-item">
                  <Link
                    to="/casefile/casefileview"
                    className={`nav-link px-2 text-dark sidebar-hover ${
                      isActive("/casefile/casefileview")
                        ? "custom-active-link"
                        : ""
                    }`}
                  >
                    Upload Document
                  </Link>
                </li>

                 <li className="nav-item">
                  <Link
                    to="/casefile/managecourt"
                    className={`nav-link px-2 text-dark sidebar-hover ${
                      isActive("/casefile/managecourt")
                        ? "custom-active-link"
                        : ""
                    }`}
                  >
                    Manage Benches
                  </Link>
                </li>

                 <li className="nav-item">
                  <Link
                    to="/casefile/causelistfile"
                    className={`nav-link px-2 text-dark sidebar-hover ${
                      isActive("/casefile/causelistfile")
                        ? "custom-active-link"
                        : ""
                    }`}
                  >
                    Download Cause List File
                  </Link>
                </li>
                <li className="nav-item">
                  <Link
                    to="/casefile/uploadmedia"
                    className={`nav-link px-2 text-dark sidebar-hover ${
                      isActive("/casefile/uploadmedia")
                        ? "custom-active-link"
                        : ""
                    }`}
                  >
                    Upload Media File
                  </Link>
                </li>
              </ul>
            )}
          </li>

          {/* Scheduling Dropdown */}
          <li className="nav-item">
            <div className="nav-link d-flex justify-content-between fw-bold align-items-center text-dark sidebar-hover">
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
                    className={`nav-link px-2 text-dark sidebar-hover ${
                      isActive("/scheduling/court-calendar")
                        ? "custom-active-link"
                        : ""
                    }`}
                  >
                    Court Calendar
                  </Link>
                </li>
                <li className="nav-item">
                  <Link
                    to="/scheduling/hearing-schedule"
                    className={`nav-link px-2 text-dark sidebar-hover ${
                      isActive("/scheduling/hearing-schedule")
                        ? "custom-active-link"
                        : ""
                    }`}
                  >
                    Hearing Schedule
                  </Link>
                </li>
                <li className="nav-item">
                  <Link
                    to="/scheduling/set-appointment"
                    className={`nav-link px-2 text-dark sidebar-hover ${
                      isActive("/scheduling/set-appointment")
                        ? "custom-active-link"
                        : ""
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
