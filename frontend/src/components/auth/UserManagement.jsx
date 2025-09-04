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
  Modal,
} from "reactstrap";
import { FaEdit, FaTrash, FaPlus } from "react-icons/fa";
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


//    Fullstack Java Developer Vijay Chaurasiya

  const { token, logout } = useAuth();
  const navigate = useNavigate();

  const [users, setUsers] = useState([]);
  const [currentPage, setCurrentPage] = useState(1);
  const [pageData, setPageData] = useState({ totalPages: 0, currentPage: 0 });
  const [loading, setLoading] = useState(true);
  const [editingUser, setEditingUser] = useState(null);
  const [showConfirmModal, setShowConfirmModal] = useState(false);
  const [pendingToggleUser, setPendingToggleUser] = useState(null);

  const usersPerPage = 5;

  const refreshUsers = useCallback(() => {
    setLoading(true);
    fetchUsers(currentPage - 1, usersPerPage, setUsers, setPageData, token).finally(() =>
      setLoading(false)
    );
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
                <th>Enable/Disable</th>
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
                          className="px-1 mb-1 me-1"
                          onClick={() => setEditingUser(user)}
                        >
                          <FaEdit />
                        </Button>

                        <Switch
                          checked={user.status}
                          onChange={() => {
                            setPendingToggleUser(user);
                            setShowConfirmModal(true);
                          }}
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

      {/* Modal for Activation/Deactivation Confirmation */}
      <Modal isOpen={showConfirmModal} toggle={() => setShowConfirmModal(false)}>
        <div className="modal-header">
          <h5 className="modal-title">
            {pendingToggleUser?.status ? "Deactivate User" : "Activate User"}
          </h5>
          <button
            type="button"
            className="btn-close"
            onClick={() => setShowConfirmModal(false)}
          ></button>
        </div>
        <div className="modal-body">
          Are you sure you want to {pendingToggleUser?.status ? "deactivate" : "activate"}{" "}
          <strong>{pendingToggleUser?.name}</strong>?
        </div>
        <div className="modal-footer">
          <Button color="secondary" onClick={() => setShowConfirmModal(false)}>
            Cancel
          </Button>
          <Button
            color={pendingToggleUser?.status ? "danger" : "success"}
            onClick={() => {
              toggleUserStatus(pendingToggleUser);
              setShowConfirmModal(false);
              setPendingToggleUser(null);
            }}
          >
            Yes, {pendingToggleUser?.status ? "Deactivate" : "Activate"}
          </Button>
        </div>
      </Modal>
    </>
  );
};

export default UserManagement;
