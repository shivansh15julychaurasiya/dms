import React, { useEffect, useState } from "react";
import {
  Card,
  CardBody,
  Table,
  Input,
  Button,
  Row,
  Col,
  Spinner,
  Modal,
  ModalHeader,
  ModalBody,
  ModalFooter,
  Form,
  FormGroup,
  Label,
} from "reactstrap";
import Select from "react-select";

import { motion } from "framer-motion";

import Sidebar from "./Sidebar";
import Topbar from "./Topbar";

import { getAllUsers } from "../services/UserService";

export default function UserManagement() {
  const [collapsed, setCollapsed] = useState(false);
  const [isMobileOpen, setIsMobileOpen] = useState(false);

  const [users, setUsers] = useState([]);
  const [search, setSearch] = useState("");

  const [pageNumber, setPageNumber] = useState(0);
  const [pageSize] = useState(5);
  const [totalPages, setTotalPages] = useState(0);
  const [lastPage, setLastPage] = useState(false);

  const [loading, setLoading] = useState(false);

  // **Modal state**
  const [isModalOpen, setIsModalOpen] = useState(false);

  // **Form fields inside modal**
  const [newUserName, setNewUserName] = useState("");
  const [newUserPhone, setNewUserPhone] = useState("");
  const [newUserRoles, setNewUserRoles] = useState([]);

  // All available roles
  const allRoles = [
    { id: 1, name: "ADMIN" },
    { id: 2, name: "USER" },
    { id: 3, name: "MANAGER" },
    { id: 4, name: "SUPERADMIN" },
  ];

  const toggleSidebar = () => {
    if (window.innerWidth < 992) {
      setIsMobileOpen(!isMobileOpen);
    } else {
      setCollapsed(!collapsed);
    }
  };

  useEffect(() => {
    loadUsers(pageNumber);
  }, [pageNumber]);

  const loadUsers = async (page) => {
    try {
      setLoading(true);
      const response = await getAllUsers(page, pageSize);
      const data = response?.data;

      setUsers(data?.content || []);
      setPageNumber(data?.pageNumber ?? 0);
      setTotalPages(data?.totalPages ?? 1);
      setLastPage(data?.lastPage ?? true);
    } catch (err) {
      console.error("Error loading users:", err);
    } finally {
      setLoading(false);
    }
  };

  const filteredUsers = users.filter((u) =>
    u.name?.toLowerCase().includes(search.toLowerCase())
  );

  const handlePrev = () => {
    if (pageNumber > 0) setPageNumber(pageNumber - 1);
  };

  const handleNext = () => {
    if (!lastPage) setPageNumber(pageNumber + 1);
  };

  // **Modal toggle function**
  const toggleModal = () => {
    setIsModalOpen(!isModalOpen);
  };

  // **Handle form submit inside modal**
  const handleAddUser = () => {
    console.log("Adding user:", {
      name: newUserName,
      phone: newUserPhone,
      roles: newUserRoles,
    });

    // Reset form
    setNewUserName("");
    setNewUserPhone("");
    setNewUserRoles([]);

    // Close modal
    toggleModal();

    // loadUsers(pageNumber);
  };

  return (
    <div className="dashboard-container d-flex">
      <Sidebar
        collapsed={collapsed}
        isMobileOpen={isMobileOpen}
        setIsMobileOpen={setIsMobileOpen}
      />

      <div className="dashboard-main flex-grow-1">
        <Topbar toggleSidebar={toggleSidebar} />

        <motion.div
          initial={{ opacity: 0, y: 20 }}
          animate={{ opacity: 1, y: 0 }}
          transition={{ duration: 0.4 }}
          className="p-4"
        >
          <Card className="shadow rounded-4">
            <CardBody>
              <h2 className="fw-bold mb-3">User Management</h2>

              <Row className="mb-3">
                <Col md="4">
                  <Input
                    type="text"
                    placeholder="Search users..."
                    value={search}
                    onChange={(e) => setSearch(e.target.value)}
                    className="rounded-3"
                  />
                </Col>

                <Col md="2">
                  <Button
                    color="primary"
                    className="rounded-3 w-100"
                    onClick={() => loadUsers(pageNumber)}
                    disabled={loading}
                  >
                    {loading ? "Loading..." : "Refresh"}
                  </Button>
                </Col>

                {/* New Add User Button */}
                <Col md="2">
                  <Button
                    color="success"
                    className="rounded-3 w-100"
                    onClick={toggleModal}
                  >
                    + Add User
                  </Button>
                </Col>
              </Row>

              {loading ? (
                <div className="text-center py-5">
                  <Spinner color="primary" />
                </div>
              ) : (
                <>
                  <Table bordered hover responsive className="rounded-3">
                    <thead className="table-light">
                      <tr>
                        <th>ID</th>
                        <th>Name</th>
                        <th>Phone</th>
                        <th>Roles</th>
                      </tr>
                    </thead>

                    <tbody>
                      {users.length > 0 ? (
                        users.map((user) => (
                          <tr key={user.user_id}>
                            <td>{user.user_id}</td>
                            <td>{user.username}</td>
                            <td>{user.phone}</td>
                            <td>
                              {user.roles?.map((role, idx) => (
                                <span
                                  key={idx}
                                  className="badge bg-primary rounded-pill px-3 py-2 me-1"
                                >
                                  {role.role_name}
                                </span>
                              ))}
                            </td>
                          </tr>
                        ))
                      ) : (
                        <tr>
                          <td
                            colSpan="4"
                            className="text-center text-muted py-3"
                          >
                            No users found
                          </td>
                        </tr>
                      )}
                    </tbody>
                  </Table>

                  <div className="d-flex justify-content-between align-items-center mt-3">
                    <Button
                      color="secondary"
                      className="rounded-3"
                      disabled={pageNumber === 0}
                      onClick={handlePrev}
                    >
                      ◀ Prev
                    </Button>

                    <strong>
                      Page {pageNumber + 1} of {totalPages}
                    </strong>

                    <Button
                      color="secondary"
                      className="rounded-3"
                      disabled={lastPage}
                      onClick={handleNext}
                    >
                      Next ▶
                    </Button>
                  </div>
                </>
              )}
            </CardBody>
          </Card>

          {/* Modal for adding user */}
          <Modal isOpen={isModalOpen} toggle={toggleModal} className="mt-5">
            <ModalHeader toggle={toggleModal}>Add New User</ModalHeader>
            <ModalBody>
              <Form>
                <FormGroup>
                  <Label for="name">Name</Label>
                  <Input
                    id="name"
                    type="text"
                    value={newUserName}
                    onChange={(e) => setNewUserName(e.target.value)}
                    placeholder="Enter name"
                  />
                </FormGroup>

                <FormGroup>
                  <Label for="phone">Phone</Label>
                  <Input
                    id="phone"
                    type="text"
                    value={newUserPhone}
                    onChange={(e) => setNewUserPhone(e.target.value)}
                    placeholder="Enter phone number"
                  />
                </FormGroup>

                {/* MULTI-SELECT ROLES */}
                <FormGroup>
                  <Label>Select Roles</Label>

                  <Select
                    isMulti
                    options={allRoles.map((r) => ({
                      value: r.name,
                      label: r.name,
                    }))}
                    value={newUserRoles.map((r) => ({ value: r, label: r }))}
                    onChange={(selected) =>
                      setNewUserRoles(selected.map((sel) => sel.value))
                    }
                    className="basic-multi-select"
                    classNamePrefix="select"
                    placeholder="Select roles..."
                  />
                </FormGroup>
              </Form>
            </ModalBody>
            <ModalFooter>
              <Button color="primary" onClick={handleAddUser}>
                Add User
              </Button>
              <Button color="secondary" onClick={toggleModal}>
                Cancel
              </Button>
            </ModalFooter>
          </Modal>
        </motion.div>
      </div>
    </div>
  );
}
