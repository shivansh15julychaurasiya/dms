import React, { useEffect, useState } from "react";
import {
  Button,
  Table,
  Pagination,
  PaginationItem,
  PaginationLink,
  Alert,
} from "reactstrap";
import Sidebar from "../layout/Sidebar";
import Navbar from "../layout/Navbar";
import "bootstrap/dist/css/bootstrap.min.css";
import { isTokenExpired } from "../../services/axios";
import { useNavigate } from "react-router-dom";
import { fetchUsers, deleteUser } from "../../services/axios";

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
    <div className="home">
      <Sidebar />
      <div className="homeContainer">
        <Navbar />
        <div className="container p-4">
          <div className="d-flex justify-content-between align-items-center mb-4">
            <h4 className="text-primary">Admin User Management</h4>
            <Button color="primary" onClick={() => navigate("/home/register")}>
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
                      <Button
                        color="warning"
                        size="sm"
                        className="me-2"
                        onClick={() => console.log("Edit user:", user)}
                      >
                        Edit
                      </Button>
                      <Button
                        color="danger"
                        size="sm"
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
            <Pagination className="justify-content-center">
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
        </div>
      </div>
    </div>
  );
};

export default AdminDashboard;
