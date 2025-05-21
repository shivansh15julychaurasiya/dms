import React, { useState, useEffect, useCallback } from "react";
import Switch from "@mui/material/Switch";

import {
  Button,
  Table,
  Pagination,
  PaginationItem,
  PaginationLink,
  Card,
  CardBody,
  Spinner,
} from "reactstrap";
import {
  FaEdit,
  FaTrash,
  FaPlus,
  FaToggleOn,
  FaToggleOff,
} from "react-icons/fa";
import { useNavigate } from "react-router-dom";
import { toast } from "react-toastify";

import { useAuth } from "../../context/AuthContext";
import {
  isTokenExpired,
  fetchUsers,
  activateUser,
  deactivateUser,
} from "../../services/userService";
import Register from "./Register";

const UserManagement = () => {
  const { token, logout } = useAuth();
  const navigate = useNavigate();

  const [users, setUsers] = useState([]);
  const [currentPage, setCurrentPage] = useState(1);
  const [pageData, setPageData] = useState({ totalPages: 0, currentPage: 0 });
  const [loading, setLoading] = useState(true);
  const [editingUser, setEditingUser] = useState(null);

  const usersPerPage = 5;

  const refreshUsers = useCallback(() => {
    setLoading(true);
    fetchUsers(
      currentPage - 1,
      usersPerPage,
      setUsers,
      setPageData,
      token
    ).finally(() => setLoading(false));
  }, [currentPage, token]);

  useEffect(() => {
    if (!token || isTokenExpired(token)) {
      logout();
      navigate("/login");
    } else {
      setLoading(true);
      refreshUsers();
    }

    const intervalId = setInterval(() => {
      if (!token || isTokenExpired(token)) {
        logout();
        navigate("/login");
      }
    }, 20000);

    return () => clearInterval(intervalId);
  }, [token, logout, navigate, currentPage, refreshUsers]);

  const deleteHandler = async (userId) => {
    await deleteUser(userId, users, setUsers, navigate);
    if (editingUser?.userId === userId) {
      setEditingUser(null);
    }
    refreshUsers();
  };

  const toggleUserStatus = async (user) => {
    try {
      const isActive = user.status;

      if (isActive) {
        await deactivateUser(user.username, token);
        toast.info(`${user.name} deactivated successfully`);
      } else {
        await activateUser(user.username, token);
        toast.success(`${user.name} activated successfully`);
      }

      // Update the user status locally to reflect in UI instantly
      setUsers((prevUsers) =>
        prevUsers.map((u) =>
          u.username === user.username ? { ...u, status: !isActive } : u
        )
      );
    } catch (error) {
      console.error("Toggle status error:", error);
      toast.error(
        error?.response?.data?.message || "Failed to toggle user status"
      );
    }
  };

  return (
    <>
      {!editingUser ? (
        <>
          <div className="d-flex justify-content-between align-items-center mt-1 mb-2">
            <Button
              color="primary"
              onClick={() => setEditingUser({})}
              className="rounded-pill"
            >
              <FaPlus className="me-2" />
              Create User
            </Button>
          </div>
          <Table responsive bordered hover>
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
                    <Spinner color="success" />
                    Loading...
                  </td>
                </tr>
              ) : users.length > 0 ? (
                users.map((user) => {
                  const isActive = user.roles.some((role) => role.status);
                  return (
                    <tr key={user.user_id}>
                      <td>{user.user_id}</td>
                      <td>{user.name}</td>
                      <td>{user.email}</td>
                      <td>{user.username}</td>
                      <td>{user.phone}</td>
                      <td>{user.about}</td>
                      <td>
                        {user.roles
                          .filter((role) => role.status)
                          .map((role) => role.role_name.replace("ROLE_", ""))
                          .join(", ") || "No Active Role"}
                      </td>
                      <td>
                        <Button
                          color="warning"
                          size=""
                          className="px-1 mb-1 me-1"
                          onClick={() => setEditingUser(user)}
                        >
                          <FaEdit />
                        </Button>

                        <Switch
                          checked={user.status}
                          onChange={() => toggleUserStatus(user)}
                          size="small"
                        />
                      </td>
                    </tr>
                  );
                })
              ) : (
                <tr>
                  <td colSpan="8" className="text-center">
                    No users found
                  </td>
                </tr>
              )}
            </tbody>
          </Table>

          {pageData.totalPages > 1 && (
            <Pagination className="justify-content-center mt-4">
              <PaginationItem disabled={currentPage === 1}>
                <PaginationLink
                  previous
                  onClick={() => setCurrentPage((p) => Math.max(p - 1, 1))}
                />
              </PaginationItem>
              {[...Array(pageData.totalPages)].map((_, i) => (
                <PaginationItem key={i} active={currentPage === i + 1}>
                  <PaginationLink onClick={() => setCurrentPage(i + 1)}>
                    {i + 1}
                  </PaginationLink>
                </PaginationItem>
              ))}
              <PaginationItem disabled={currentPage === pageData.totalPages}>
                <PaginationLink
                  next
                  onClick={() =>
                    setCurrentPage((p) => Math.min(p + 1, pageData.totalPages))
                  }
                />
              </PaginationItem>
            </Pagination>
          )}
        </>
      ) : (
        <Card className="mt-4">
          <CardBody>
            <Register
              user={Object.keys(editingUser).length === 0 ? null : editingUser}
              setEditingUser={setEditingUser}
              refreshUsers={refreshUsers}
            />
          </CardBody>
        </Card>
      )}
    </>
  );
};

export default UserManagement;
