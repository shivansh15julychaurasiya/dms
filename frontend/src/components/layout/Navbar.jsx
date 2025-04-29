import React, { useEffect, useState } from "react";
import { FaUserCircle } from "react-icons/fa";
import {
  Navbar,
  NavbarBrand,
  Container,
  Button,
  Modal,
  ModalHeader,
  ModalBody,
} from "reactstrap";
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
            <div className="d-none d-md-inline text-dark small">{formattedDateTime}</div>

            <Button
              color="link"
              className="p-0"
              onClick={toggleModal}
              title="View Profile"
            >
              <FaUserCircle size={26} className="text-primary" />
            </Button>

            <span className="fw-semibold d-none d-md-inline  text-dark">E-High Court</span>
          </div>
        </Container>
      </Navbar>

      <div className="mb-1" style={{ height: "60px" }}></div>

      {/* Right Corner Profile Modal */}
      <Modal
        isOpen={modalOpen}
        toggle={toggleModal}
        backdrop={true}
        className="custom-right-modal"
        modalClassName="custom-modal-content"
      >
        <ModalHeader toggle={toggleModal} className="border-0 text-dark text-center fw-bold">
          User Profile
        </ModalHeader>
        <ModalBody className="p-">
          <ProfileCard />
        </ModalBody>
      </Modal>
    </>
  );
};

export default CustomNavbar;
