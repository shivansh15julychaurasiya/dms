import { ROLE_PATHS } from "./constants";
import { toast } from "react-toastify";
import axios from "axios";
// import { axiosInstance } from "../services/userService";
// export const getRoleRedirectPath = (role) => {
//   return ROLE_PATHS[role] || "/home/unauthorize";
// };

// Define role priority (highest first)
const ROLE_PRIORITY = ["DMSAdmin", "ROLE_USER", "ROLE_ECOURT"];

export const getRoleRedirectPath = (roleNames) => {
  console.log("redirect role="+roleNames)
  for (let role of ROLE_PRIORITY) {
    if (roleNames.includes(role)) {
      return ROLE_PATHS[role];  // return path for the first matched role
    }
  }
  // fallback route if no known role matched
  return "/home/userdashboard";
};


// Helper: Token expiration
export const isTokenExpired = (token) => {
  try {
    const decoded = JSON.parse(atob(token.split(".")[1]));
    return decoded.exp < Math.floor(Date.now() / 1000);
  } catch {
    return true;
  }
};
const axiosInstance = axios.create({
  baseURL: "http://localhost:8080/dms", // adjust as needed
});


axiosInstance.interceptors.request.use(
  (config) => {
    const token = localStorage.getItem("token");
    if (token) config.headers.Authorization = `Bearer ${token}`;
    return config;
  },
  (error) => Promise.reject(error)
);

axiosInstance.interceptors.response.use(
  (response) => response,
  (error) => {
    if (error.response?.status === 401) {
      // Token is expired/invalid → logout user
      localStorage.removeItem("token");
      localStorage.removeItem("user");
      window.location.href = "/dms/"; // or navigate to login page
    }
    return Promise.reject(error);
  }
);





// showAlert function that handles different types of alerts
export const showAlert = (msg, type = "error") => {
  if (type === "error") {
    toast.error(msg, {
      position: "top-center",
      autoClose: 1000, // Adjust auto-close time as needed
      hideProgressBar: false,
      closeOnClick: true,
      pauseOnHover: true,
      draggable: true,
    });
  } else if (type === "success") {
    toast.success(msg, {
      position: "top-center",
      autoClose: 1000, // Adjust auto-close time as needed
      hideProgressBar: false,
      closeOnClick: true,
      pauseOnHover: true,
      draggable: true,
    });
  } else if (type === "info") {
    toast.info(msg, {
      position: "top-right",
      autoClose: 3000,
      hideProgressBar: false,
      closeOnClick: true,
      pauseOnHover: true,
      draggable: true,
    });
  } else if (type === "warn") {
    toast.warn(msg, {
      position: "top-right",
      autoClose: 3000,
      hideProgressBar: false,
      closeOnClick: true,
      pauseOnHover: true,
      draggable: true,
    });
  }
};

