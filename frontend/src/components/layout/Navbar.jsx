import React, { useEffect, useState } from "react";
import { FaUserCircle } from "react-icons/fa";
import { Navbar, NavbarBrand, Container, Button, Modal } from "reactstrap";
import ProfileCard from "../profile/ProfileCard";
import "../../assets/styles.css"; // Ensure your shimmer and modal classes are here

const CustomNavbar = () => {
  const [currentDateTime, setCurrentDateTime] = useState(new Date());
  const [modalOpen, setModalOpen] = useState(false);

  useEffect(() => {
    const interval = setInterval(() => {
      setCurrentDateTime(new Date());
    }, 1000);
    return () => clearInterval(interval);
  }, []);

  const toggleModal = () => setModalOpen(!modalOpen);

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
        <Container className="d-flex justify-content-between align-items-center px-5">
          <NavbarBrand className="fw-bold text-primary">High Court</NavbarBrand>

          <div className="d-flex align-items-center gap-3">
            <div className="d-none d-md-inline text-dark small">
              {formattedDateTime}
            </div>

            <Button
              color="link"
              className="p-0"
              onClick={toggleModal}
              title="View Profile"
            >
              <FaUserCircle size={30} className="text-primary" />
            </Button>

            {/* <span className="fw-semibold d-none d-md-inline text-dark">
              E-High Court
            </span> */}
          </div>
        </Container>
      </Navbar>

      <div className="mb-1" style={{ height: "40px" }}></div>

      {/* Right Corner Profile Modal */}
      <Modal
        isOpen={modalOpen}
        toggle={toggleModal}
        backdrop={false} // Remove background overlay
        className="custom-right-modal"
        modalClassName="custom-modal-content"
        style={{
          borderRadius: "15px", // Add rounded corners to modal
          maxHeight:"200px",
          maxWidth:"200px",
          padding: 0, // Ensure no extra padding around the modal
        }}
      >
        {/* Pass the toggleModal function as a prop to ProfileCard */}
        <ProfileCard toggleModal={toggleModal} />
      </Modal>
    </>
  );
};

export default CustomNavbar;
