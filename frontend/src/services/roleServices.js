import { showAlert } from "../utils/helpers";
import { ROLE_API_PATHS } from "../utils/constants";
import { axiosInstance } from './userService';  // Correctly import axiosInstance

// Fetch all roles
export const fetchRoles = async ( setRoles, token) => {

//    Fullstack Java Developer Vijay Chaurasiya

  try {
    const res = await axiosInstance.get(ROLE_API_PATHS.ROLES, {
     
      headers: {
        Authorization: `Bearer ${token}`,
      },
    });

    setRoles(res.data.data.content);
    // console.log(res.data.data.content)
  
  } catch (error) {
    console.error("Failed to fetch roles:", error);
  }
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
export const assignRoleToUser = async (username, roleId, token) => {
  // console.log(loginId+roleId())
  const response = await axiosInstance.get(
    `/role/assign-role`,
    {
      params: { username, roleId },
      headers: {
        Authorization: `Bearer ${token}`,
        "Content-Type": "application/json",
      },
    }
  );
  return response.data;
};



// Deassign role from user (fixed to accept token)
export const deassignRoleFromUser = async (username, roleId, token) => {
  return await axiosInstance.get(
    `/role/deassign-role`,
    {
      params: {
        username,
        roleId,
      },
      headers: {
        Authorization: `Bearer ${token}`,
        "Content-Type": "application/json",
      },
    }
  );
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
export const createRole = async (newRole, token) => {
  try {
    const response = await axiosInstance.post(
      ROLE_API_PATHS.CREATE_ROLE,
      { role_name: newRole },
      {
        headers: {
          Authorization: `Bearer ${token}`,
          "Content-Type": "application/json",
        },
      }
    );
    return response.data; // return the API response
  } catch (error) {
    console.log(error);
    // Rethrow for the calling function to handle the error
  }
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
