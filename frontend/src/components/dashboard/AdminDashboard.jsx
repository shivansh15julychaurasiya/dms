import React, { useEffect, useState } from "react";
import {
  Button,
  Table,
  Pagination,
  PaginationItem,
  PaginationLink,
  Container,
  Row,
  Col,
} from "reactstrap";

import Sidebar from "../layout/Sidebar"; // Sidebar component
import Navbar from "../layout/Navbar"; // Top navigation bar
import { isTokenExpired, fetchUsers, deleteUser } from "../../services/userService"; // API helper functions
import { useNavigate } from "react-router-dom"; // For programmatic navigation
import { useAuth } from "../../context/AuthContext"; // Auth context to access token and logout
import { FaEdit, FaTrash } from "react-icons/fa"; // Add this at the top


const AdminDashboard = () => {
  const { token, logout } = useAuth(); // Access token and logout function from context
  const [users, setUsers] = useState([]); // State to store list of users
  const [currentPage, setCurrentPage] = useState(1); // Current pagination page
  const usersPerPage = 5; // Users to display per page
  const [loading, setLoading] = useState(true); // State to handle loading

  

  const navigate = useNavigate();

  // On component mount, check token validity and fetch users
  useEffect(() => {
    if (!token || isTokenExpired(token)) {
      logout(); // Logout if token is missing or expired
      navigate("/login"); // Redirect to login page
    } else {
      // Fetch users from backend and set loading state
      fetchUsers(setUsers, token);
      setLoading(false);
    }

    // Periodic token check every 10 seconds
    const intervalId = setInterval(() => {
      if (!token || isTokenExpired(token)) {
        logout(); // Logout if token expires during session
        navigate("/login"); // Redirect to login page
      }
    }, 20000);

    return () => clearInterval(intervalId); // Cleanup interval on unmount
  }, [token, logout, navigate]);

  // Delete user handler
  const deleteHandler = async (userId) => {
    await deleteUser(userId, users, setUsers, navigate);
  };

  // Pagination logic
  const indexOfLastUser = currentPage * usersPerPage;
  const indexOfFirstUser = indexOfLastUser - usersPerPage;
  const currentUsers = Array.isArray(users)
    ? users.slice(indexOfFirstUser, indexOfLastUser)
    : [];
  const totalPages = Math.ceil(users.length / usersPerPage);

  return (
    <div className="d-flex">
      {/* Sidebar for admin navigation */}
      <Sidebar />

      <div className="flex-grow-1">
        {/* Top Navbar */}
        <Navbar />

        {/* Main content container */}
        <Container fluid className="">
          {/* Header with title and create user button */}
          <Row className="align-items-center ">
            <Col xs="12" md="6">
              <h4 className="text-primary">Admin User Management</h4>
            </Col>
            <Col xs="12" md="6" className="text-md-end mt-2 mt-md-0">
              <Button
                color="primary"
                onClick={() => navigate("/home/register")}
              >
                Create User
              </Button>
            </Col>
          </Row>

          {/* Users table */}
          {loading ? (
            <div className="text-center">Loading...</div>
          ) : (
            <Table responsive bordered hover className="mt-2">
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
                              .map((role) =>
                                role.role_name.replace("ROLE_", "")
                              )
                              .join(", ")
                          : "No Role"}
                      </td>

                      {/* // Inside your JSX: */}
                      <td>
                        <Button
                          color="warning"
                          size="sm"
                          className="px-1 mb-1 me-1"
                          onClick={() =>
                            navigate(`/home/updateuser/${user.userId}`)
                          }
                        >
                          <FaEdit />
                        </Button>
                        <Button
                          color="danger"
                          size="sm"
                          className=" px-1 mb-1"
                          onClick={() => deleteHandler(user.userId)}
                        >
                          <FaTrash />
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
          )}

          {/* Pagination controls */}
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
