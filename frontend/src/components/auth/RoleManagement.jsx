// src/components/admin/RoleManagement.js

import {
  Table,
  Form,
  Row,
  Col,
  Card,
  CardBody,
  FormGroup,
  Label,
  Input,
  Button,
  Alert,
} from "reactstrap";
import { FaPlus, FaUserMinus, FaTrash, FaEdit } from "react-icons/fa";
import { useState, useEffect } from "react";
import {
  fetchRoles,
  createRole,
  deleteRole,
  deassignRoleFromUser,
} from "../../services/roleServices";
import { useAuth } from "../../context/AuthContext";
import { showAlert } from "../../utils/helpers";

const RoleManagement = ({ onRolesUpdate }) => {
  const [roles, setRoles] = useState([]);
  const [newRole, setNewRole] = useState("");
  const [loading, setLoading] = useState(true);
  const [userId, setUserId] = useState("");
  const [deassignRoleId, setDeassignRoleId] = useState("");

  const { token } = useAuth();

  useEffect(() => {
    fetchRoles(token, setRoles);
    setLoading(false);
  }, [token]);

  useEffect(() => {
    // When roles are fetched or updated, pass them to the parent
    if (roles.length > 0) {
      onRolesUpdate(roles); // Pass roles back to parent
    }
  }, [roles, onRolesUpdate]);

  const handleCreateRole = async () => {
    console.log(newRole + token);
    if (newRole.trim()) {
      await createRole(newRole, token);
    }
  };

  const handleDeleteRole = async (roleId) => {
    await deleteRole(roleId, setRoles);
  };

  const handleDeassignRole = async () => {
    if (userId && deassignRoleId) {
      try {
        await deassignRoleFromUser(Number(userId), Number(deassignRoleId));
        showAlert("Role de-assigned successfully.");
        setUserId("");
        setDeassignRoleId("");
      } catch (err) {
        showAlert("Failed to de-assign role.");
      }
    } else {
      showAlert("Please fill all fields.");
    }
  };

  return (
    <div>
      <h5 className="text-primary fw-bold mb-4">Role Management</h5>

      <Row>
        {/* Create Role - Left Column */}
        <Col md={6}>
          <Card className="shadow-sm bg-light-subtle">
            <CardBody>
              <h6 className="text-success mb-3">
                <FaPlus className="me-2" />
                Create New Role
              </h6>
              <Form
                onSubmit={(e) => {
                  e.preventDefault();
                  handleCreateRole();
                }}
              >
                <FormGroup>
                  <Label for="roleName">Role Name</Label>
                  <Input
                    id="roleName"
                    value={newRole}
                    onChange={(e) => setNewRole(e.target.value)}
                    placeholder="e.g., role_admin"
                  />
                </FormGroup>
                <Button color="primary" type="submit">
                  <FaPlus className="me-2" />
                  Create Role
                </Button>
              </Form>
            </CardBody>
          </Card>
        </Col>

        {/* De-assign Role - Right Column */}
        <Col md={6}>
          <Card className="shadow-sm bg-light-subtle">
            <CardBody>
              <h6 className="text-danger mb-3">
                <FaUserMinus className="me-2" />
                De-assign Role from User
              </h6>
              <Form
                onSubmit={(e) => {
                  e.preventDefault();
                  handleDeassignRole();
                }}
              >
                <FormGroup>
                  <Label for="userId">User ID</Label>
                  <Input
                    id="userId"
                    type="number"
                    placeholder="Enter User ID"
                    value={userId}
                    onChange={(e) => setUserId(e.target.value)}
                  />
                </FormGroup>

                <FormGroup>
                  <Label for="roleSelect">Select Role</Label>
                  <Input
                    id="roleSelect"
                    type="select"
                    value={deassignRoleId}
                    onChange={(e) => setDeassignRoleId(e.target.value)}
                  >
                    <option value="">Select a Role</option>
                    {roles.map((role) => (
                      <option key={role.roleId} value={role.roleId}>
                        {role.role_name.replace("ROLE_", "")}
                      </option>
                    ))}
                  </Input>
                </FormGroup>

                <Button color="danger" type="submit">
                  De-assign Role
                </Button>
              </Form>
            </CardBody>
          </Card>
        </Col>
      </Row>

      {/* Role List Table */}
      {loading ? (
        <div className="text-center mt-4">Loading roles...</div>
      ) : (
        <Table responsive bordered hover className="mt-4 ">
          <thead className="table-dark table-sm text-center">
            <tr>
              <th>#</th>
              <th>Role Name</th>
              <th>Actions</th>
            </tr>
          </thead>
          <tbody className="text-center">
            {roles.length > 0 ? (
              roles.map((role, index) => (
                <tr key={role.roleId}>
                  <td>{index + 1}</td>
                  <td>{role.role_name.replace("ROLE_", "")}</td>
                  <td>
                    <Button
                      color="success px-2 me-2"
                      size="sm"
                      onClick={() => handleDeleteRole(role.roleId)}
                    >
                      <FaEdit />
                    </Button>
                    <Button
                      color="danger"
                      size="sm"
                      onClick={() => handleDeleteRole(role.roleId)}
                    >
                      <FaTrash />
                    </Button>
                  </td>
                </tr>
              ))
            ) : (
              <tr>
                <td colSpan="3" className="text-center">
                  No roles found
                </td>
              </tr>
            )}
          </tbody>
        </Table>
      )}
    </div>
  );
};

export default RoleManagement;
