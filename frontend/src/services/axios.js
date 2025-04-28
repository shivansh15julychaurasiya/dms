// axios.js (refactored with utils/constants extraction)

import axios from "axios";
import {  showAlert } from "../utils/helpers";
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
export const loginUser = async (loginId, password) => {
  try {
    const response = await axiosInstance.post(API_PATHS.LOGIN, {
      username: loginId,
      password,
    });

    // console.log("Login Response:", response.data.token); // <-- ADD THIS
    return response.data.data;
  } catch (error) {
    console.error("Login failed:", error.response || error.message);
    throw new Error("Login failed");
  }
};



// export const loginUser = async (loginId, password) => {
//   const response = await axiosInstance.post(API_PATHS.LOGIN, {
//     username: loginId,
//     password,
//   });
//   console.log(response.data.data)
//   return response.data.data;
// };


// Fetch user by ID (renamed to getUserById if needed)
export const fetchUserById = (userId,token) => {
  if (!window.confirm("Are you sure you want to update this user?")) return;

  axiosInstance
    .get(API_PATHS.GET_USER(userId), {
      headers: {
        Authorization: `Bearer ${token}`,
      },
    })
    .catch((err) => handleError(err, "Could not fetch user."));
};

// Fetch all users
export const fetchUsers = (setUsers,token) => {
  // const token = localStorage.getItem("token");
  // if (!token || isTokenExpired(token)) {
  //   localStorage.removeItem("token");
  //   navigate("/home/login");
  //   return;
  // }

  axiosInstance
    .get(API_PATHS.USERS, {
      headers: { Authorization: `Bearer ${token}` },
    })
    .then((res) => setUsers(res.data.data))
    .catch(() => showAlert("Error loading users. You may not be authorized."))
};

// Delete user
export const deleteUser = (userId, users, setUsers) => {
  showAlert("Are you sure you want to delete this user?")
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
export const saveUser = async (userData, navigate, token) => {
  try {
    console.log(userData);
    const response = await axiosInstance.post(API_PATHS.CREATE_USER, userData, {
      headers: {
        Authorization: `Bearer ${token}`, // Add token in the Authorization header
      },
    });
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
export { axiosInstance };  // Named export