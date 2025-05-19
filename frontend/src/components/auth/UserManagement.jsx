import React, { useState, useEffect } from "react";
import {
  Button,
  Table,
  Pagination,
  PaginationItem,
  PaginationLink,
  Card,
  CardBody,
} from "reactstrap";
import { FaEdit, FaTrash, FaPlus } from "react-icons/fa";
import { useNavigate } from "react-router-dom";
import { useAuth } from "../../context/AuthContext";
import {
  isTokenExpired,
  fetchUsers,
  deleteUser,
} from "../../services/userService";
import Register from "./Register";

const UserManagement = () => {
  const { tokenLog, logout } = useAuth();
  const [users, setUsers] = useState([]);
  const [currentPage, setCurrentPage] = useState(1);
  const [loading, setLoading] = useState(true);
  const [editingUser, setEditingUser] = useState(null); // null = not editing
  const usersPerPage = 5;

  const navigate = useNavigate();

  useEffect(() => {
    if (!tokenLog || isTokenExpired(tokenLog)) {
      logout();
      navigate("/login");
    } else {
      fetchUsers(setUsers, tokenLog);
      setLoading(false);
    }

    const intervalId = setInterval(() => {
      if (!tokenLog || isTokenExpired(tokenLog)) {
        logout();
        navigate("/login");
      }
    }, 20000);

    return () => clearInterval(intervalId);
  }, [tokenLog, logout, navigate]);

  const indexOfLastUser = currentPage * usersPerPage;
  const indexOfFirstUser = indexOfLastUser - usersPerPage;
  const currentUsers = users.slice(indexOfFirstUser, indexOfLastUser);
  const totalPages = Math.ceil(users.length / usersPerPage);

  const deleteHandler = async (userId) => {
    await deleteUser(userId, users, setUsers, navigate);
    if (editingUser?.userId === userId) {
      setEditingUser(null);
    }
  };

  return (
    <>
      {!editingUser ? (
        <>
          <div className="d-flex justify-content-between align-items-center mt-1 mb-2">
          <Button color="primary"  onClick={() => setEditingUser({})} className="rounded-pill">
              <FaPlus className="me-2 " />
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
                  <td colSpan="8" className="text-center">Loading...</td>
                </tr>
              ) : currentUsers.length > 0 ? (
                currentUsers.map((user) => (
                  <tr key={user.userId}>
                    <td>{user.userId}</td>
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
                        onClick={() => setEditingUser(user)}
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
                  <td colSpan="8" className="text-center">No users found</td>
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
        </>
      ) : (
        <Card className="mt-4">
          <CardBody>
            <Register
              user={Object.keys(editingUser).length === 0 ? null : editingUser}
              setEditingUser={setEditingUser}
              refreshUsers={() => fetchUsers(setUsers, tokenLog)}
            />
          </CardBody>
        </Card>
      )}
    </>
  );
};

export default UserManagement;
