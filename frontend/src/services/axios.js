// axios.js (refactored with utils/constants extraction)

import axios from "axios";
import { getRoleRedirectPath, showAlert } from "../utils/helpers";
import { API_BASE_URL, API_PATHS } from "../utils/constants";

// Axios instance
const axiosInstance = axios.create({
  baseURL: API_BASE_URL,
  headers: {
    "Content-Type": "application/json",
  },
});

// Helper: Token expiration
export const isTokenExpired = (token) => {
  try {
    const decoded = JSON.parse(atob(token.split(".")[1]));
    return decoded.exp < Math.floor(Date.now() / 1000);
  } catch {
    return true;
  }
};

// Reusable error handler
const handleError = (err, fallbackMsg) => {
  console.error(fallbackMsg, err);
  showAlert(fallbackMsg);
};

// Login user
export const loginUser = (loginId, password, setErrorMsg, navigate) => {
  axiosInstance
    .post(API_PATHS.LOGIN, { username: loginId, password })
    .then((res) => {
      const { token, user } = res.data.data;
      const role = user.roles[0].name.trim();

      localStorage.setItem("token", token);
      localStorage.setItem("user", JSON.stringify(user));
      localStorage.setItem("role", role);

      navigate(getRoleRedirectPath(role));
    })
    .catch(() => {
      setErrorMsg("Invalid email or password");
      handleError(null, "Login failed.");
    });
};

// Fetch user by ID (renamed to getUserById if needed)
export const fetchUserById = (userId) => {
  if (!window.confirm("Are you sure you want to update this user?")) return;

  axiosInstance
    .get(API_PATHS.GET_USER(userId), {
      headers: {
        Authorization: `Bearer ${localStorage.getItem("token")}`,
      },
    })
    .catch((err) => handleError(err, "Could not fetch user."));
};

// Fetch all users
export const fetchUsers = (setUsers, setError, navigate) => {
  const token = localStorage.getItem("token");
  if (!token || isTokenExpired(token)) {
    localStorage.removeItem("token");
    navigate("/home/login");
    return;
  }

  axiosInstance
    .get(API_PATHS.USERS, {
      headers: { Authorization: `Bearer ${token}` },
    })
    .then((res) => setUsers(res.data))
    .catch(() => setError("Error loading users. You may not be authorized."));
};

// Delete user
export const deleteUser = (userId, users, setUsers) => {
  if (!window.confirm("Are you sure you want to delete this user?")) return;

  axiosInstance
    .delete(API_PATHS.DELETE_USER(userId), {
      headers: {
        Authorization: `Bearer ${localStorage.getItem("token")}`,
      },
    })
    .then(() => {
      setUsers(users.filter((user) => user.userId !== userId));
    })
    .catch((err) => handleError(err, "Could not delete user."));
};

// Register/save user
export const saveUser = async (userData, navigate) => {
  try {
    const response = await axiosInstance.post(API_PATHS.REGISTER, userData);
    console.log("User created:", response.data);
    navigate("/home/admindashboard");
  } catch (error) {
    const message = error.response?.data?.message || "Failed to save user.";
    throw new Error(message);
  }
};

// Update user
export const updateUser = (userData, setUsers, setError, setShowModal, navigate) => {
  const token = localStorage.getItem("token");

  axiosInstance
    .put(API_PATHS.UPDATE_USER(userData.userId), userData, {
      headers: {
        Authorization: `Bearer ${token}`,
      },
    })
    .then(() => fetchUsers(setUsers, setError, navigate))
    .then(() => setShowModal(false))
    .catch((err) => handleError(err, "Failed to update user."));
};

// Send OTP
export const requestOtp = (loginId, setMessage, setOtpSent, setTimer) => {
  axiosInstance
    .post(API_PATHS.REQUEST_OTP, {
      login_id: loginId,
      otp_type: "Reset",
    })
    .then(() => {
      setMessage("OTP has been sent successfully!");
      setTimeout(() => setMessage(""), 3000);
      setOtpSent(true);
      setTimer(120);
    })
    .catch(() => {
      setMessage("Failed to send OTP. Please check the Login ID.");
    });
};

// Verify OTP
export const verifyOtp = (loginId, otp, setMessage, navigate) => {
  axiosInstance
    .post(API_PATHS.VERIFY_OTP, {
      username: loginId,
      otp,
    })
    .then((res) => {
      const token = res.data.data.token;
      localStorage.setItem("token", token);
      navigate("/home/reset");
      setMessage("OTP has been verified successfully!");
    })
    .catch(() => {
      setMessage("Failed to verify OTP. Please check the Login ID.");
    });
};

// Reset password
export const resetPassword = async (loginId, newPassword, token) => {
  try {
    const response = await axiosInstance.post(
      API_PATHS.RESET_PASSWORD,
      {
        login_id: loginId,
        password: newPassword,
      },
      {
        headers: {
          Authorization: `Bearer ${token}`,
        },
      }
    );
    console.log("Password reset successful:", response.data);
    return response.data;
  } catch (error) {
    console.error("Failed to reset password:", error);
    throw new Error(error.response?.data?.message || "Password reset failed.");
  }
};
