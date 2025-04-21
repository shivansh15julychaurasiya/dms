import React, { useEffect, useState } from "react";
import Sidebar from "./sidebar/Sidebar";
import Navbar from "./navbar/Navbar";
import "bootstrap/dist/css/bootstrap.min.css";
import { Modal, Button, Form, Row, Col } from "react-bootstrap";
import "../../src/assets/styles.css";
import { isTokenExpired } from "../auth/authUtils";
import { useNavigate } from "react-router-dom";
import { fetchUsers, deleteUser, handleFormSubmit } from '../services/axios'; // Import the functions

const UserDashboard = () => {
  const [users, setUsers] = useState([]);
  const [error, setError] = useState("");
  const [currentPage, setCurrentPage] = useState(1);
  const [showModal, setShowModal] = useState(false);
  const [modalMode, setModalMode] = useState("view");
  const [selectedUser, setSelectedUser] = useState(null);

  const navigate = useNavigate();
  const usersPerPage = 5;

  useEffect(() => {
    const checkTokenAndFetch = () => {
      const token = localStorage.getItem("token");
      if (!token || isTokenExpired(token)) {
        localStorage.removeItem("token");
        navigate("/home/login");
      } else {
        fetchUsers(setUsers, setError, navigate); // Fetch users
      }
    };

    // Initial check
    checkTokenAndFetch();

    // Re-check every 10 seconds
    const intervalId = setInterval(() => {
      const token = localStorage.getItem("token");
      if (!token || isTokenExpired(token)) {
        localStorage.removeItem("token");
        navigate("/home/login");
      }
    }, 10000);

    return () => clearInterval(intervalId);
  }, []);

  const deleteHandler = async (userId) => {
    await deleteUser(userId, users, setUsers, setError, navigate); // Call the delete function
  };

  const handleView = (user) => {
    setSelectedUser(user);
    setModalMode("view");
    setShowModal(true);
  };

  const handleEdit = (user) => {
    setSelectedUser({ ...user });
    setModalMode("edit");
    setShowModal(true);
  };

  const handleCreate = () => {
    setSelectedUser({
      name: "",
      email: "",
      about: "",
      password: "",
      phone: "",
      login_id: "",
    });
    setModalMode("create");
    setShowModal(true);
  };

  const handleFormChange = (e) => {
    const { name, value } = e.target;
    setSelectedUser((prev) => ({ ...prev, [name]: value }));
  };

  const handleFormSubmitLocal = async (e) => {
    await handleFormSubmit(e, selectedUser, modalMode, users, setUsers, setError, setShowModal); // Submit the form
  };

  const indexOfLastUser = currentPage * usersPerPage;
  const indexOfFirstUser = indexOfLastUser - usersPerPage;
  const currentUsers = users.slice(indexOfFirstUser, indexOfLastUser);
  const totalPages = Math.ceil(users.length / usersPerPage);

  const paginate = (pageNumber) => setCurrentPage(pageNumber);
  const nextPage = () => setCurrentPage((prev) => Math.min(prev + 1, totalPages));
  const prevPage = () => setCurrentPage((prev) => Math.max(prev - 1, 1));

  return (
    <div className="home">
      <Sidebar />
      <div className="homeContainer">
        <Navbar />
        <div className="container-fluid p-4">
          <div className="d-flex justify-content-between align-items-center mb-3">
            <h4 className="shimmer-text">Admin User Management</h4>
            <Button variant="primary" onClick={handleCreate}>
              Create User
            </Button>
          </div>

          {error && <div className="alert alert-danger">{error}</div>}

          <div className="table-responsive">
            <table className="table table-striped table-hover table-bordered">
              <thead className="table-dark">
                <tr>
                  <th>#</th>
                  <th>Name</th>
                  <th>Email</th>
                  <th>Employee ID</th>
                  <th>Phone</th>
                  <th>About</th>
                  <th>Role</th>
                  <th>Actions</th>
                </tr>
              </thead>
              <tbody>
                {currentUsers.length > 0 ? (
                  currentUsers.map((user, index) => (
                    <tr key={user.userId}>
                      <td>{indexOfFirstUser + index + 1}</td>
                      <td>{user.name}</td>
                      <td>{user.email}</td>
                      <td>{user.login_id}</td>
                      <td>{user.phone}</td>
                      <td>{user.about}</td>
                      <td>
                        {user.roles && user.roles.length > 0 ? (
                          user.roles.map((role, idx) => (
                            <span key={role.id}>
                              {role.name.replace("ROLE_", "")}
                              {idx < user.roles.length - 1 && ", "}
                            </span>
                          ))
                        ) : (
                          <span className="text-muted">No role</span>
                        )}
                      </td>
                      <td>
                        <button
                          className="btn btn-info btn-sm px-2 me-2"
                          onClick={() => handleView(user)}
                          title="View"
                        >
                          <i className="bi bi-eye"></i>
                        </button>
                        <button
                          className="btn btn-warning btn-sm px-2 me-2"
                          onClick={() => handleEdit(user)}
                          title="Edit"
                        >
                          <i className="bi bi-pencil-square"></i>
                        </button>
                        <button
                          className="btn btn-danger btn-sm px-2"
                          onClick={() => deleteHandler(user.userId)}
                          title="Delete"
                        >
                          <i className="bi bi-trash"></i>
                        </button>
                      </td>
                    </tr>
                  ))
                ) : (
                  <tr>
                    <td colSpan="8" className="text-center">
                      No users found
                    </td>
                  </tr>
                )}
              </tbody>
            </table>
          </div>

          {totalPages > 1 && (
            <nav>
              <ul className="pagination justify-content-center">
                <li className={`page-item ${currentPage === 1 ? "disabled" : ""}`}>
                  <button className="page-link" onClick={prevPage}>
                    Previous
                  </button>
                </li>
                {[...Array(totalPages)].map((_, i) => (
                  <li
                    key={i + 1}
                    className={`page-item ${currentPage === i + 1 ? "active" : ""}`}
                  >
                    <button className="page-link" onClick={() => paginate(i + 1)}>
                      {i + 1}
                    </button>
                  </li>
                ))}
                <li className={`page-item ${currentPage === totalPages ? "disabled" : ""}`}>
                  <button className="page-link" onClick={nextPage}>
                    Next
                  </button>
                </li>
              </ul>
            </nav>
          )}
        </div>
      </div>

      {/* Modal */}
      <Modal show={showModal} onHide={() => setShowModal(false)} centered>
        <Modal.Header closeButton>
          <Modal.Title className="shimmer-text">
            {modalMode === "view"
              ? "User Details"
              : modalMode === "edit"
              ? "Edit User"
              : "Create User"}
          </Modal.Title>
        </Modal.Header>
        <Modal.Body>
          {selectedUser && (
            <Form onSubmit={handleFormSubmitLocal}>
              <Form.Group className="mb-3">
                <Row>
                  <Col>
                    <Form.Label>Name</Form.Label>
                    <Form.Control
                      name="name"
                      value={selectedUser.name}
                      onChange={handleFormChange}
                      disabled={modalMode === "view"}
                      required
                    />
                  </Col>
                  <Col>
                    <Form.Label>Email</Form.Label>
                    <Form.Control
                      type="email"
                      name="email"
                      value={selectedUser.email}
                      onChange={handleFormChange}
                      disabled={modalMode === "view"}
                      required
                    />
                  </Col>
                </Row>
              </Form.Group>

              <Form.Group className="mb-3">
                <Row>
                  <Col>
                    <Form.Label>Phone</Form.Label>
                    <Form.Control
                      name="phone"
                      value={selectedUser.phone}
                      onChange={handleFormChange}
                      disabled={modalMode === "view"}
                      required={modalMode === "create"}
                    />
                  </Col>
                  <Col>
                    <Form.Label>Employee ID (login_id)</Form.Label>
                    <Form.Control
                      name="login_id"
                      value={selectedUser.login_id}
                      onChange={handleFormChange}
                      disabled={modalMode === "view"}
                      required={modalMode === "create"}
                    />
                  </Col>
                </Row>
              </Form.Group>

              <Form.Group className="mb-3">
                <Row>
                  <Col>
                    <Form.Label>About</Form.Label>
                    <Form.Control
                      as="textarea"
                      name="about"
                      value={selectedUser.about}
                      onChange={handleFormChange}
                      disabled={modalMode === "view"}
                    />
                  </Col>
                </Row>
              </Form.Group>

              {modalMode !== "view" && (
                <Button variant="primary" type="submit">
                  {modalMode === "edit" ? "Save Changes" : "Create User"}
                </Button>
              )}
            </Form>
          )}
        </Modal.Body>
      </Modal>
    </div>
  );
};

export default UserDashboard;
