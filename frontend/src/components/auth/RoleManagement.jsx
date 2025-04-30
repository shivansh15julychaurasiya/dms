// src/components/admin/RoleManagement.js

import React, { useState, useEffect } from "react";
import { Button, Input, Table, FormGroup, Label } from "reactstrap";
import { FaPlus, FaTrash } from "react-icons/fa"; // Icons for adding and deleting roles
import { fetchRoles, createRole, deleteRole, assignRoleToUser } from "../../services/roleServices"; // API helper functions

const RoleManagement = () => {
  const [roles, setRoles] = useState([]);
  const [newRole, setNewRole] = useState("");
  const [selectedRole, setSelectedRole] = useState("");
  const [loading, setLoading] = useState(true);

  // Fetch roles on component mount
  useEffect(() => {
    fetchRoles(setRoles, setLoading);
  }, []);

  // Handle role creation
  const handleCreateRole = async () => {
    if (newRole.trim()) {
      await createRole(newRole, setRoles, setNewRole);
    }
  };

  // Handle role deletion
  const handleDeleteRole = async (roleId) => {
    await deleteRole(roleId, setRoles);
  };

  // Handle assigning role to a user
  const handleAssignRole = async () => {
    if (selectedRole.trim()) {
      await assignRoleToUser(selectedRole);
    }
  };

  return (
    <div>
      <h5>Role Management</h5>
      
      {/* Create New Role */}
      <FormGroup>
        <Label for="roleName">New Role Name</Label>
        <Input
          id="roleName"
          value={newRole}
          onChange={(e) => setNewRole(e.target.value)}
        />
        <Button
          color="primary"
          onClick={handleCreateRole}
          className="mt-2"
        >
          <FaPlus /> Create Role
        </Button>
      </FormGroup>

      {/* Assign Role to User */}
      <FormGroup>
        <Label for="roleAssign">Assign Role to User</Label>
        <Input
          id="roleAssign"
          type="select"
          value={selectedRole}
          onChange={(e) => setSelectedRole(e.target.value)}
        >
          <option value="">Select a Role</option>
          {roles.map((role) => (
            <option key={role.roleId} value={role.roleId}>
              {role.role_name.replace("ROLE_", "")}
            </option>
          ))}
        </Input>
        <Button
          color="success"
          onClick={handleAssignRole}
          className="mt-2"
        >
          Assign Role
        </Button>
      </FormGroup>

      {/* Role List */}
      {loading ? (
        <div className="text-center"></div>
      ) : (
        <Table responsive bordered hover className="mt-3">
          <thead className="table-dark">
            <tr>
              <th>#</th>
              <th>Role Name</th>
              <th>Actions</th>
            </tr>
          </thead>
          <tbody>
            {roles.length > 0 ? (
              roles.map((role, index) => (
                <tr key={role.roleId}>
                  <td>{index + 1}</td>
                  <td>{role.role_name.replace("ROLE_", "")}</td>
                  <td>
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
