import React, { useEffect, useState } from "react";
import {
  Modal,
  ModalHeader,
  ModalBody,
  Button,
  Form,
  FormGroup,
  Label,
  Input,
  Row,
  Col,
  Table,
  Pagination,
  PaginationItem,
  PaginationLink,
  Alert,
} from "reactstrap";
import Sidebar from "./sidebar/Sidebar";
import Navbar from "./navbar/Navbar";
import "bootstrap/dist/css/bootstrap.min.css";
import { isTokenExpired } from "../auth/authUtils";
import { useNavigate } from "react-router-dom";
import { fetchUsers, deleteUser, handleFormSubmit } from "../services/axios";

const AdminDashboard = () => {
  const [users, setUsers] = useState([]);
  const [error, setError] = useState("");
  const [currentPage, setCurrentPage] = useState(1);
  const [showModal, setShowModal] = useState(false);
  const [modalMode, setModalMode] = useState("view");
  const [selectedUser, setSelectedUser] = useState(null);

  const navigate = useNavigate();
  const usersPerPage = 5;

  useEffect(() => {
    const token = localStorage.getItem("token");
    if (!token || isTokenExpired(token)) {
      localStorage.removeItem("token");
      navigate("/home/login");
    } else {
      fetchUsers(setUsers, setError, navigate);
    }

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
    await deleteUser(userId, users, setUsers, setError, navigate);
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
      loginId: "",
    });
    setModalMode("create");
    setShowModal(true);
  };

  const handleFormChange = (e) => {
    const { name, value } = e.target;
    setSelectedUser((prev) => ({ ...prev, [name]: value }));
  };

  const handleFormSubmitLocal = async (e) => {
    await handleFormSubmit(e, selectedUser, modalMode, users, setUsers, setError, setShowModal);
  };

  const indexOfLastUser = currentPage * usersPerPage;
  const indexOfFirstUser = indexOfLastUser - usersPerPage;
  const currentUsers = users.slice(indexOfFirstUser, indexOfLastUser);
  const totalPages = Math.ceil(users.length / usersPerPage);

  return (
    <div className="home">
      <Sidebar />
      <div className="homeContainer">
        <Navbar />
        <div className="container p-4">
          <div className="d-flex justify-content-between align-items-center mb-4">
            <h4 className="text-primary">Admin User Management</h4>
            <Button color="primary" onClick={handleCreate}>
              Create User
            </Button>
          </div>

          {error && <Alert color="danger">{error}</Alert>}

          <Table responsive hover bordered>
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
                      {user.roles && user.roles.length > 0
                        ? user.roles.map((role, idx) => (
                            <span key={role.id}>
                              {role.name.replace("ROLE_", "")}
                              {idx < user.roles.length - 1 && ", "}
                            </span>
                          ))
                        : "No Role"}
                    </td>
                    <td>
                      <Button color="info" size="sm" className="me-2" onClick={() => handleView(user)}>
                        View
                      </Button>
                      <Button color="warning" size="sm" className="me-2" onClick={() => handleEdit(user)}>
                        Edit
                      </Button>
                      <Button color="danger" size="sm" onClick={() => deleteHandler(user.userId)}>
                        Delete
                      </Button>
                    </td>
                  </tr>
                ))
              ) : (
                <tr>
                  <td colSpan="8" className="text-center">No users found</td>
                </tr>
              )}
            </tbody>
          </Table>

          {totalPages > 1 && (
            <Pagination className="justify-content-center">
              <PaginationItem disabled={currentPage === 1}>
                <PaginationLink previous onClick={() => setCurrentPage((p) => Math.max(p - 1, 1))} />
              </PaginationItem>
              {[...Array(totalPages)].map((_, i) => (
                <PaginationItem key={i} active={currentPage === i + 1}>
                  <PaginationLink onClick={() => setCurrentPage(i + 1)}>{i + 1}</PaginationLink>
                </PaginationItem>
              ))}
              <PaginationItem disabled={currentPage === totalPages}>
                <PaginationLink next onClick={() => setCurrentPage((p) => Math.min(p + 1, totalPages))} />
              </PaginationItem>
            </Pagination>
          )}
        </div>
      </div>

      {/* Modal */}
      <Modal isOpen={showModal} toggle={() => setShowModal(false)} centered>
        <ModalHeader toggle={() => setShowModal(false)}>
          {modalMode === "view" ? "User Details" : modalMode === "edit" ? "Edit User" : "Create User"}
        </ModalHeader>
        <ModalBody>
          {selectedUser && (
            <Form onSubmit={handleFormSubmitLocal}>
              <FormGroup>
                <Label for="name">Name</Label>
                <Input name="name" value={selectedUser.name} onChange={handleFormChange} disabled={modalMode === "view"} required />
              </FormGroup>
              <FormGroup>
                <Label for="email">Email</Label>
                <Input type="email" name="email" value={selectedUser.email} onChange={handleFormChange} disabled={modalMode === "view"} required />
              </FormGroup>
              <FormGroup>
                <Label for="phone">Phone</Label>
                <Input name="phone" value={selectedUser.phone} onChange={handleFormChange} disabled={modalMode === "view"} required={modalMode === "create"} />
              </FormGroup>
              <FormGroup>
                <Label for="loginId">Employee ID (Login ID)</Label>
                <Input name="loginId" value={selectedUser.loginId} onChange={handleFormChange} disabled={modalMode === "view"} required={modalMode === "create"} />
              </FormGroup>
              <FormGroup>
                <Label for="about">About</Label>
                <Input type="textarea" name="about" value={selectedUser.about} onChange={handleFormChange} disabled={modalMode === "view"} />
              </FormGroup>
              {modalMode !== "view" && (
                <Button color="primary" type="submit">
                  {modalMode === "edit" ? "Save Changes" : "Create User"}
                </Button>
              )}
            </Form>
          )}
        </ModalBody>
      </Modal>
    </div>
  );
};

export default AdminDashboard;
