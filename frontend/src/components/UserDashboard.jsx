import React, { useEffect, useState } from "react";
import Sidebar from "./sidebar/Sidebar";
import Navbar from "./navbar/Navbar";
import "bootstrap/dist/css/bootstrap.min.css";
import { Modal, Button, Form, Row, Col } from "react-bootstrap";
import "../../src/assets/styles.css";
import { isTokenExpired } from "../auth/authUtils";
import { useNavigate } from "react-router-dom";

const UserDashboard = () => {
  const [users, setUsers] = useState([]);
  const [error, setError] = useState("");
  const [currentPage, setCurrentPage] = useState(1);
  const [showModal, setShowModal] = useState(false);
  const [modalMode, setModalMode] = useState("view");
  const [selectedUser, setSelectedUser] = useState(null);

  const navigate = useNavigate();
  const usersPerPage = 5;

  const fetchUsers = async () => {
    try {
      const token = localStorage.getItem("token");
      if (!token || isTokenExpired(token)) {
        localStorage.removeItem("token");
        navigate("dms/home/login");
        return;
      }

      const res = await fetch("http://localhost:8081/dms/users/", {
        headers: {
          Authorization: `Bearer ${token}`,
        },
      });

      if (res.status === 401) {
        localStorage.removeItem("token");
        navigate("dms/home/login");
        return;
      }

      if (!res.ok) throw new Error("Failed to fetch users");

      const data = await res.json();
      setUsers(data);
    } catch (err) {
      console.error(err.message);
      setError("Error loading users. You may not be authorized.");
    }
  };

  useEffect(() => {
    const checkTokenAndFetch = () => {
      const token = localStorage.getItem("token");
      if (!token || isTokenExpired(token)) {
        localStorage.removeItem("token");
        navigate("/home/login");
      } else {
        fetchUsers();
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

  const deleteUser = async (userId) => {
    if (!window.confirm("Are you sure you want to delete this user?")) return;
    try {
      const res = await fetch(`http://localhost:8081/dms/users/${userId}`, {
        method: "DELETE",
        headers: {
          Authorization: `Bearer ${localStorage.getItem("token")}`,
        },
      });

      if (!res.ok) throw new Error("Failed to delete user");

      setUsers(users.filter((user) => user.userId !== userId));
    } catch (err) {
      console.error(err.message);
      alert("Could not delete user.");
    }
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

  const handleFormSubmit = async (e) => {
    e.preventDefault();

    const token = localStorage.getItem("token");

    const url =
      modalMode === "edit"
        ? `http://localhost:8081/dms/users/${selectedUser.userId}`
        : "http://localhost:8081/dms/auth/register";

    const method = modalMode === "edit" ? "PUT" : "POST";

    const headers = {
      "Content-Type": "application/json",
    };

    if (modalMode === "edit") {
      headers.Authorization = `Bearer ${token}`;
    }

    try {
      const res = await fetch(url, {
        method,
        headers,
        body: JSON.stringify(selectedUser),
      });

      if (!res.ok) {
        if (res.status === 401) {
          setError("Unauthorized. You do not have permission to create a user.");
        } else {
          setError("Failed to save user.");
        }
        throw new Error("Failed to save user");
      }

      fetchUsers();
      setShowModal(false);
    } catch (err) {
      console.error(err.message);
      alert("Failed to save user.");
    }
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
                          onClick={() => deleteUser(user.userId)}
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
            <Form onSubmit={handleFormSubmit}>
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
                      name="about"
                      value={selectedUser.about}
                      onChange={handleFormChange}
                      disabled={modalMode === "view"}
                    />
                  </Col>

                  {modalMode === "create" && (
                    <Col>
                      <Form.Label>Password</Form.Label>
                      <Form.Control
                        type="password"
                        name="password"
                        value={selectedUser.password}
                        onChange={handleFormChange}
                        required
                      />
                    </Col>
                  )}
                </Row>
              </Form.Group>

              {modalMode !== "view" && (
                <div className="d-flex justify-content-end">
                  <Button variant="primary" type="submit">
                    {modalMode === "edit" ? "Update" : "Create"}
                  </Button>
                </div>
              )}
            </Form>
          )}
        </Modal.Body>
      </Modal>
    </div>
  );
};

export default UserDashboard;
