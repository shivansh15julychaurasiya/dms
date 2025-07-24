// utils/constants.js

export const API_BASE_URL = "http://localhost:8081/dms";

// Auth/User API paths
export const API_PATHS = {
  LOGIN: "/auth/login-password",
  USERS: "/users/",
  CREATE_USER: "/users/create",
  DELETE_USER: (id) => `/users/${id}`,
  UPDATE_USER: (id) => `/users/${id}`,
  GET_USER: (id) => `/users/${id}`,

  REQUEST_OTP: "/auth/request-otp",
  VERIFY_OTP: "/auth/verify-forgot-otp",
  RESET_PASSWORD: "/auth/change-password/forgot",
  CHANGE_PASSWORD: "/auth/change-password/reset",

  ACTIVATE_USER: (id) => `/users/activate/${id}`,
  DEACTIVATE_USER: (id) => `/users/deactivate/${id}`,
};

// Role-related API paths
export const ROLE_API_PATHS = {
  ROLES: "/role/",
  GET_ROLE: (roleId) => `/role/${roleId}`,
  CREATE_ROLE: "/role/",
  UPDATE_ROLE: (roleId) => `/role/${roleId}/update`,
  DELETE_ROLE: (roleId) => `/role/${roleId}/delete`,
  ASSIGN_ROLE: "/role/assign",
  REMOVE_ROLE: (userId, roleId) => `/role/${roleId}/remove/${userId}`,
};

// Case type-related API paths
export const CASE_TYPE_API_PATHS = {
  CASE_TYPES: "/api/casetypes",
};

// Cause list API paths
export const CAUSE_LIST_API_PATHS = {
  CAUSE_LIST_TYPES: "/causelisttypes",
   // Used in courtService.js
};

export const COURT_MASTER = {

  COURT_MASTER_TYPE: "/court-master-type",
  CREATE_NEW_COURT: "/court-master-type/newcourt"

}


// Role-based routing paths
export const ROLE_PATHS = {
  ROLE_ADMIN: "/home/admindashboard",
  ROLE_USER: "/home/userdashboard",
  ROLE_ECOURT: "/home/ecourtdashboard",
};
