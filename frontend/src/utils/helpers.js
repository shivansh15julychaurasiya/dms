import { ROLE_PATHS } from "./constants";

export const getRoleRedirectPath = (role) => {
  return ROLE_PATHS[role] || "/home/unauthorize";
};

export const showAlert = (message) => {
  alert(message); // Replace with toast or modal if needed
};
