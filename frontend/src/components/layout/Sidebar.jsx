import React, { useState } from "react";
import { Link } from "react-router-dom";
import { FaFileAlt, FaCalendarAlt, FaChartBar, FaBars } from "react-icons/fa";
import "../../assets/styles.css";
import { useAuth } from "../../context/AuthContext";


const Sidebar = () => {
  const [isOpen, setIsOpen] = useState(true);
  const [activeDropdown, setActiveDropdown] = useState("");
    const { role,token } = useAuth();
  

  // const role = localStorage.getItem("role"); // get role directly
  console.log(token+role)

  const toggleSidebar = () => setIsOpen((prev) => !prev);
  const toggleDropdown = (item) =>
    setActiveDropdown((prev) => (prev === item ? "" : item));

  return (
    <>
      {/* Sidebar */}
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
        <div className="text-center py-3 border-bottom">
          {isOpen && <h5 className="text-primary m-0">High Court</h5>}
        </div>

        <ul className="nav flex-column mt-3">
          {/* Home Dropdown */}
          <li className="nav-item">
            <div
              className="nav-link d-flex justify-content-between align-items-center text-dark hover-effect"
              onClick={() => toggleDropdown("Home")}
              style={{ cursor: "pointer" }}
            >
              <span>
                <FaFileAlt className="me-2" />
                {isOpen && "Home"}
              </span>
              {isOpen && (
                <span>{activeDropdown === "Home" ? "▲" : "▼"}</span>
              )}
            </div>
            {activeDropdown === "Home" && isOpen && (
              <ul className="nav flex-column ms-3">
                <li className="nav-item">
                  <Link to="/home/dashboard" className="nav-link text-dark px-2">
                    Dashboard
                  </Link>
                </li>
                <li className="nav-item">
                  <Link to="/home/search" className="nav-link text-dark px-2">
                    Search
                  </Link>
                </li>
                {role === "ROLE_ADMIN" && (
                  <li className="nav-item">
                    <Link to="/home/admindashboard" className="nav-link text-dark px-2">
                      Manage Users
                    </Link>
                  </li>
                )}
                <li className="nav-item">
                  <Link to="/home/managecauselist" className="nav-link text-dark px-2">
                    Manage CauseList
                  </Link>
                </li>
                <li className="nav-item">
                  <Link to="/cases_mgmt/assignedCase" className="nav-link text-dark px-2">
                    Reserved Cases
                  </Link>
                </li>
                <li className="nav-item">
                  <Link to="/cases_mgmt/status" className="nav-link text-dark px-2">
                    Status
                  </Link>
                </li>
              </ul>
            )}
          </li>

          {/* Static Link */}
          <li className="nav-item">
            <Link
              to="/analytics"
              className="nav-link text-dark d-flex align-items-center hover-effect"
            >
              <FaChartBar className="me-2" />
              {isOpen && "Analytics"}
            </Link>
          </li>

          {/* Scheduling Dropdown */}
          <li className="nav-item">
            <div
              className="nav-link d-flex justify-content-between align-items-center text-dark hover-effect"
              onClick={() => toggleDropdown("Scheduling")}
              style={{ cursor: "pointer" }}
            >
              <span>
                <FaCalendarAlt className="me-2" />
                {isOpen && "Scheduling"}
              </span>
              {isOpen && (
                <span>{activeDropdown === "Scheduling" ? "▲" : "▼"}</span>
              )}
            </div>
            {activeDropdown === "Scheduling" && isOpen && (
              <ul className="nav flex-column ms-3">
                {["Court Calendar", "Hearing Schedule", "Set Appointment"].map(
                  (item, idx) => (
                    <li className="nav-item" key={idx}>
                      <a
                        href={`#${item.toLowerCase().replace(/ /g, "-")}`}
                        className="nav-link text-dark px-2"
                      >
                        {item}
                      </a>
                    </li>
                  )
                )}
              </ul>
            )}
          </li>
        </ul>
      </div>

      {/* Toggle Button */}
      <button
        className="btn btn-primary position-fixed top-0 start-0 m-2"
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
