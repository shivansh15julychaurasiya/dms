export const API_BASE_URL = "http://localhost:8081/dms";


export const API_PATHS = {
  LOGIN: "/auth/login-password",
  REGISTER: "/auth/register",
  USERS: "/users/",
  DELETE_USER: (id) => `/users/${id}`,
  UPDATE_USER: (id) => `/users/${id}`,
  REQUEST_OTP: "/auth/request-otp",
  VERIFY_OTP: "/auth/verify-reset-otp",
  RESET_PASSWORD: "/home/reset", // for frontend navigation
};

export const ROLE_PATHS = {
  ROLE_ADMIN: "/home/admindashboard",
  ROLE_USER: "/home/userdashboard",
  ROLE_ECOURT: "/home/ecourtdashboard",
};
