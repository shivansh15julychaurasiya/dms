import React, { useEffect, useState } from "react";
import {
  Button,
  Table,
  Pagination,
  PaginationItem,
  PaginationLink,
  Alert,
  Container,
  Row,
  Col,
} from "reactstrap";
import Sidebar from "../layout/Sidebar";
import Navbar from "../layout/Navbar";
import "bootstrap/dist/css/bootstrap.min.css";
import { isTokenExpired, fetchUsers, deleteUser } from "../../services/axios";
import { useNavigate } from "react-router-dom";

const AdminDashboard = () => {
  const [users, setUsers] = useState([]);
  const [error, setError] = useState("");
  const [currentPage, setCurrentPage] = useState(1);

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

  const indexOfLastUser = currentPage * usersPerPage;
  const indexOfFirstUser = indexOfLastUser - usersPerPage;
  const currentUsers = users.slice(indexOfFirstUser, indexOfLastUser);
  const totalPages = Math.ceil(users.length / usersPerPage);

  return (
    <div className="d-flex">
      <Sidebar />
      <div className="flex-grow-1">
        <Navbar />
        <Container fluid className="p-4">
          <Row className="align-items-center mb-3">
            <Col xs="12" md="6">
              <h4 className="text-primary">Admin User Management</h4>
            </Col>
            <Col xs="12" md="6" className="text-md-end mt-2 mt-md-0">
              <Button color="primary" onClick={() => navigate("/home/register")}>
                Create User
              </Button>
            </Col>
          </Row>

          {error && <Alert color="danger">{error}</Alert>}

          <Table responsive bordered hover className="mt-3">
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
                      {user.roles?.length
                        ? user.roles
                            .map((role) => role.name.replace("ROLE_", ""))
                            .join(", ")
                        : "No Role"}
                    </td>
                    <td>
                      <Button
                        color="warning"
                        size="sm"
                        className="me-2 mb-1"
                        onClick={() => console.log("Edit user:", user)}
                      >
                        Edit
                      </Button>
                      <Button
                        color="danger"
                        size="sm"
                        className="mb-1"
                        onClick={() => deleteHandler(user.userId)}
                      >
                        Delete
                      </Button>
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
          </Table>

          {totalPages > 1 && (
            <Pagination className="justify-content-center mt-4">
              <PaginationItem disabled={currentPage === 1}>
                <PaginationLink
                  previous
                  onClick={() => setCurrentPage((p) => Math.max(p - 1, 1))}
                />
              </PaginationItem>
              {[...Array(totalPages)].map((_, i) => (
                <PaginationItem key={i} active={currentPage === i + 1}>
                  <PaginationLink onClick={() => setCurrentPage(i + 1)}>
                    {i + 1}
                  </PaginationLink>
                </PaginationItem>
              ))}
              <PaginationItem disabled={currentPage === totalPages}>
                <PaginationLink
                  next
                  onClick={() =>
                    setCurrentPage((p) => Math.min(p + 1, totalPages))
                  }
                />
              </PaginationItem>
            </Pagination>
          )}
        </Container>
      </div>
    </div>
  );
};

export default AdminDashboard;
