import { ROLE_PATHS } from "./constants";
import { toast } from "react-toastify";

// export const getRoleRedirectPath = (role) => {
//   return ROLE_PATHS[role] || "/home/unauthorize";
// };

// Define role priority (highest first)
const ROLE_PRIORITY = ["ROLE_ADMIN", "ROLE_USER", "ROLE_ECOURT"];

export const getRoleRedirectPath = (roleNames) => {
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





// showAlert function that handles different types of alerts
export const showAlert = (msg, type = "error") => {
  if (type === "error") {
    toast.error(msg, {
      position: "top-center",
      autoClose: 3000, // Adjust auto-close time as needed
      hideProgressBar: false,
      closeOnClick: true,
      pauseOnHover: true,
      draggable: true,
    });
  } else if (type === "success") {
    toast.success(msg, {
      position: "top-right",
      autoClose: 3000, // Adjust auto-close time as needed
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

