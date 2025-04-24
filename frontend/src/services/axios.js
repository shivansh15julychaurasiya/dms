// axios.js (refactored with utils/constants extraction)

import axios from "axios";
import { getRoleRedirectPath, showAlert } from "../utils/helpers";
import { API_BASE_URL,API_PATHS } from "../utils/constants";

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

  axiosInstance.post(API_PATHS.LOGIN, { username: loginId, password }).then((res)=>{
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

  // axiosInstance
  //   .post("/auth/login-password", { username: loginId, password })
  //   .then((res) => {
  //     const { token, user } = res.data.data;
  //     const role = user.roles[0].name.trim();

  //     localStorage.setItem("token", token);
  //     localStorage.setItem("user", JSON.stringify(user));
  //     localStorage.setItem("role", role);

  //     navigate(getRoleRedirectPath(role));
  //   })
  //   .catch(() => {
  //     setErrorMsg("Invalid email or password");
  //     handleError(null, "Login failed.");
  //   });
};

// Fetch users
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
console.log(localStorage.getItem("token"))
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
// Save a new user
// export const saveUser = (userData) => {

//   console.log(userData)
//   axiosInstance.post(API_PATHS.REGISTER, userData, {
//     headers: {
//       "Content-Type": "application/json",
//     },
//   })
//     // .then(() => fetchUsers(setUsers, setError, navigate))
//     .catch((err) => handleError(err, "Failed to save user."));
// };
// import axios from "axios";
// import { API_PATHS } from "./apiPaths"; // Adjust if needed


export const saveUser = async (userData, navigate) => {
  try {
    const response = await axiosInstance.post(API_PATHS.REGISTER, userData, {
      headers: {
        "Content-Type": "application/json",
      },
    });
    console.log("User created:", response.data);
    navigate("/home/admindashboard"); // Update route as needed
  } catch (error) {
    const message = error.response?.data?.message || "Failed to save user.";
    throw new Error(message);
  }
};


// Update an existing user
export const updateUser = (userData, setUsers, setError, setShowModal, navigate) => {
  const token = localStorage.getItem("token");

  axiosInstance.put(API_PATHS.UPDATE_USER(userData.userId), userData, {
    headers: {
      "Content-Type": "application/json",
      Authorization: `Bearer ${token}`,
    },
  })
    .then(() => fetchUsers(setUsers, setError, navigate))
    .then(() => setShowModal(false))
    .catch((err) => handleError(err, "Failed to update user."));
};

// Create or update user
// export const handleFormSubmit = (
//   e,
//   selectedUser,
//   mode,
//   users,
//   setUsers,
//   setError,
//   setShowModal,
//   navigate
// ) => {
//   e.preventDefault();
//   const token = localStorage.getItem("token");
//   const url = mode === "edit" ? `/users/${selectedUser.userId}` : "/auth/register";
//   const method = mode === "edit" ? "put" : "post";

//   axiosInstance({
//     method,
//     url,
//     data: selectedUser,
//     headers: {
//       "Content-Type": "application/json",
//       ...(mode === "edit" && { Authorization: `Bearer ${token}` }),
//     },
//   })
//     .then(() => fetchUsers(setUsers, setError, navigate))
//     .then(() => setShowModal(false))
//     .catch((err) => handleError(err, "Failed to save user."));
// };

// Forgot Password - Send OTP
export const requestOtp = (loginId, setMessage, setOtpSent, setTimer) => {
  axiosInstance
    .post(API_PATHS.REQUEST_OTP, {
      login_id: loginId,
      otp_type: "Reset",
    })
    .then((res) => {
      console.log(res);
      setMessage("OTP has been sent successfully!");
      setTimeout(() => setMessage(""), 3000);
      setOtpSent(true);
      setTimer(120);
    })
    .catch((err) => {
      console.error(err);
      setMessage("Failed to send OTP. Please check the Login ID.");
    });
};

// Forgot Password - Verify OTP
export const verifyOtp = (loginId, otp, setMessage, navigate) => {
  axiosInstance
    .post(API_PATHS.VERIFY_OTP, {
      username: loginId,
      otp: otp,
    })
    .then((res) => {
      const token = res.data.data.token;
      localStorage.setItem("token", token);
      navigate("/home/reset");
      setMessage("OTP has been verified successfully!");
    })
    .catch((err) => {
      console.error(err);
      setMessage("Failed to verify OTP. Please check the Login ID.");
    });
};
