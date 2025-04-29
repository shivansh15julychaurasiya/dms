import React, { useState, useEffect } from "react";
import { Button, Table, Pagination, PaginationItem, PaginationLink } from "reactstrap";
import { FaEdit, FaTrash } from "react-icons/fa"; // Add icons for actions
import { useNavigate } from "react-router-dom"; // For programmatic navigation
import { useAuth } from "../../context/AuthContext"; // Auth context to access token and logout
import { isTokenExpired, fetchUsers, deleteUser } from "../../services/userService"; // API helper functions

const UserManagement = () => {
  const { token, logout } = useAuth(); // Access token and logout function from context
  const [users, setUsers] = useState([]); // State to store users
  const [currentPage, setCurrentPage] = useState(1); // Current page for pagination
  const [loading, setLoading] = useState(true); // Loading state
  const usersPerPage = 5; // Number of users per page
  const navigate = useNavigate();

  // Fetch users on component mount and check token validity
  useEffect(() => {
    if (!token || isTokenExpired(token)) {
      logout(); // Logout if token is missing or expired
      navigate("/login"); // Redirect to login page
    } else {
      // Fetch users and set loading state
      fetchUsers(setUsers, token);
      setLoading(false);
    }

    // Periodic token check every 20 seconds
    const intervalId = setInterval(() => {
      if (!token || isTokenExpired(token)) {
        logout();
        navigate("/login");
      }
    }, 20000);

    return () => clearInterval(intervalId); // Cleanup interval on unmount
  }, [token, logout, navigate]);

  // Pagination logic
  const indexOfLastUser = currentPage * usersPerPage;
  const indexOfFirstUser = indexOfLastUser - usersPerPage;
  const currentUsers = Array.isArray(users)
    ? users.slice(indexOfFirstUser, indexOfLastUser)
    : [];
  const totalPages = Math.ceil(users.length / usersPerPage);

  // Delete user handler
  const deleteHandler = async (userId) => {
    await deleteUser(userId, users, setUsers, navigate);
  };

  return (
    <>
      {/* Users Table */}
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
          {loading ? (
            <tr>
              <td colSpan="8" className="text-center">
                Loading...
              </td>
            </tr>
          ) : users.length > 0 ? (
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
                        .map((role) => role.role_name.replace("ROLE_", ""))
                        .join(", ")
                    : "No Role"}
                </td>
                <td>
                  <Button
                    color="warning"
                    size="sm"
                    className="px-1 mb-1 me-1"
                    onClick={() => navigate(`/home/updateuser/${user.userId}`)}
                  >
                    <FaEdit />
                  </Button>
                  <Button
                    color="danger"
                    size="sm"
                    className="px-1 mb-1"
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

      {/* Pagination */}
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
    </>
  );
};

export default UserManagement;
