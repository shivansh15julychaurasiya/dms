export const API_BASE_URL = "http://localhost:8081/dms";


export const API_PATHS = {
  LOGIN: "/auth/login-password",
  USERS: "/users/",
  CREATE_USER: "/users/create", // New path for user creation
  DELETE_USER: (id) => `/users/${id}`,
  UPDATE_USER: (id) => `/users/${id}`,
  GET_USER: (id) => `/users/${id}`,

  REQUEST_OTP: "/auth/request-otp",
  VERIFY_OTP: "/auth/verify-reset-otp",
  RESET_PASSWORD: "/auth/reset-password", // for frontend navigation
};

// Role-related API paths
export const ROLE_API_PATHS = {
  // Role management
  ROLES: "/role/",// Get all roles
  GET_ROLE: (roleId) => `/role/${roleId}`,  // Get specific role by ID
  CREATE_ROLE: "/role/", // Create a new role
  UPDATE_ROLE: (roleId) => `/role/${roleId}/update`, // Update specific role by ID
  DELETE_ROLE: (roleId) => `/role/${roleId}/delete`, // Delete specific role by ID

  // User-Role management
  ASSIGN_ROLE: "/role/assign", // Assign a role to a user
  REMOVE_ROLE: (userId, roleId) => `/role/${roleId}/remove/${userId}`, // Remove role from a user
};




export const ROLE_PATHS = {
  ROLE_ADMIN: "/home/admindashboard",
  ROLE_USER: "/home/userdashboard",
  ROLE_ECOURT: "/home/ecourtdashboard",
};


