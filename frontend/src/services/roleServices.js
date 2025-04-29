import { showAlert } from "../utils/helpers";
import { ROLE_API_PATHS } from "../utils/constants";
import { axiosInstance } from './userService';  // Correctly import axiosInstance

// Fetch all roles
export const fetchRoles = ( token,setRoles) => {
    console.log(token)


  axiosInstance
    .get(ROLE_API_PATHS.ROLES, {
      headers: { Authorization: `Bearer ${token}` },
    })
    .then((res) => {
      if (res.data.status) {
        setRoles(res.data.data); // Assuming roles are in data property
      } else {
        showAlert("No roles found.");
      }
    })
    
};

// Fetch role by ID
export const fetchRoleById = (roleId, token) => {
  axiosInstance
    .get(ROLE_API_PATHS.GET_ROLE(roleId), {
      headers: { Authorization: `Bearer ${token}` },
    })
    .then((res) => {
      if (res.data.status) {
        return res.data.data; // Return the fetched role
      } else {
        showAlert("Role not found.");
      }
    })
    .catch(() => showAlert("Error fetching role."));
};

// Assign role to a user
export const assignRoleToUser = (userId, roleId, token) => {
  axiosInstance
    .post(
      ROLE_API_PATHS.ASSIGN_ROLE,
      { userId, roleId },
      {
        headers: { Authorization: `Bearer ${token}` },
      }
    )
    .then((res) => {
      if (res.data.status) {
        showAlert("Role assigned successfully.");
      } else {
        showAlert("Failed to assign role.");
      }
    })
    .catch(() => showAlert("Error assigning role to user."));
};

// Remove role from user
export const removeRoleFromUser = (userId, roleId, token) => {
  axiosInstance
    .delete(ROLE_API_PATHS.REMOVE_ROLE(userId, roleId), {
      headers: { Authorization: `Bearer ${token}` },
    })
    .then((res) => {
      if (res.data.status) {
        showAlert("Role removed successfully.");
      } else {
        showAlert("Failed to remove role.");
      }
    })
    .catch(() => showAlert("Error removing role from user."));
};

// Create a new role
export const createRole = (roleData, token) => {
  axiosInstance
    .post(ROLE_API_PATHS.CREATE_ROLE, roleData, {
      headers: { Authorization: `Bearer ${token}` },
    })
    .then((res) => {
      if (res.data.status) {
        showAlert("Role created successfully.");
      } else {
        showAlert("Failed to create role.");
      }
    })
    .catch(() => showAlert("Error creating role."));
};

// Update a role
export const updateRole = (roleId, roleData, token) => {
  axiosInstance
    .put(ROLE_API_PATHS.UPDATE_ROLE(roleId), roleData, {
      headers: { Authorization: `Bearer ${token}` },
    })
    .then((res) => {
      if (res.data.status) {
        showAlert("Role updated successfully.");
      } else {
        showAlert("Failed to update role.");
      }
    })
    .catch(() => showAlert("Error updating role."));
};

// Delete a role
export const deleteRole = (roleId, token) => {
  axiosInstance
    .delete(ROLE_API_PATHS.DELETE_ROLE(roleId), {
      headers: { Authorization: `Bearer ${token}` },
    })
    .then((res) => {
      if (res.data.status) {
        showAlert("Role deleted successfully.");
      } else {
        showAlert("Failed to delete role.");
      }
    })
    .catch(() => showAlert("Error deleting role."));
};
