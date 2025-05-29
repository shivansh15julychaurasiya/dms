import React, { useEffect, useState, useRef } from "react";
import { FaUserCircle } from "react-icons/fa";
import { Navbar, NavbarBrand, Button } from "reactstrap";
import ProfileCard from "../profile/ProfileCard";
import "../../assets/styles.css"; // Make sure your custom styles are here

const CustomNavbar = () => {
  const [currentDateTime, setCurrentDateTime] = useState(new Date());
  const [profileVisible, setProfileVisible] = useState(false);
  const profileRef = useRef();

  useEffect(() => {
    const interval = setInterval(() => {
      setCurrentDateTime(new Date());
    }, 1000);
    return () => clearInterval(interval);
  }, []);

  // Hide profile if clicked outside
  useEffect(() => {
    const handleClickOutside = (event) => {
      if (
        profileRef.current &&
        !profileRef.current.contains(event.target)
      ) {
        setProfileVisible(false);
      }
    };

    if (profileVisible) {
      document.addEventListener("mousedown", handleClickOutside);
    } else {
      document.removeEventListener("mousedown", handleClickOutside);
    }

    return () => document.removeEventListener("mousedown", handleClickOutside);
  }, [profileVisible]);

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
      <Navbar
        light
        expand="md"
        fixed="top"
        className="bg-light shadow-sm border-bottom"
        style={{ height: "50px", zIndex: 1040 }}
      >
        <NavbarBrand className="fw-bold text-primary px-5">
          Allahabad High Court
        </NavbarBrand>

        <div className="d-flex align-items-center gap-3 position-relative">
          <div className="d-none d-md-inline text-dark small">
            {formattedDateTime}
          </div>

          <Button
            color="link"
            className="p-0 px-4"
            onClick={() => setProfileVisible(!profileVisible)}
            title="View Profile"
          >
            <FaUserCircle size={30} className="text-primary" />
          </Button>

          {/* ProfileCard as Dropdown/Popover */}
          {profileVisible && (
            <div
              ref={profileRef}
              className="profile-dropdown-container"
            >
              <ProfileCard toggleModal={() => setProfileVisible(false)} />
            </div>
          )}
        </div>
      </Navbar>

      <div className="mb-1" style={{ height: "40px" }}></div>
    </>
  );
};

export default CustomNavbar;
