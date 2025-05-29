import axios from "axios";
import { showAlert } from "../utils/helpers";
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
    return response.data.data;
  } catch (error) {
    console.error("Login failed:", error.response || error.message);
    throw new Error("Login failed");
  }
};

// Fetch user by ID
export const getUserById = async (userId, token) => {
  const response = await axiosInstance.get(API_PATHS.GET_USER(userId), {
    headers: {
      Authorization: `Bearer ${token}`,
    },
  });
  return response.data;
};

export const fetchUsers = async (pageNumber, pageSize, setUsers, setPageData, token) => {
  try {
    const res = await axiosInstance.get(API_PATHS.USERS, {
      params: {
        pageNumber, // should be a number, e.g., 0-based page index
        pageSize,   // number of users per page
      },
      headers: {
        Authorization: `Bearer ${token}`,
      },
    });
    // console.log(res.data.data.totalPages)

    // Adjust according to your backend response structure:
    setUsers(res.data.data.content);
    setPageData({
      totalPages: res.data.data.totalPages,
      currentPage: res.data.data.pageNumber + 1, // if backend is zero-based page
    });
  } catch  {
    // console.error("Error fetching users");
  }
};


// Register/save user

export const saveUser = async (userData, token) => {
  try {
    const response = await axiosInstance.post(
     API_PATHS.CREATE_USER,
      userData,
      {
        headers: {
          "Content-Type": "application/json",
          Authorization: `Bearer ${token}`,
        },
      }
    );
    return response.data;
  } catch (error) {
    // console.error('saveUser error:', error.response);
    throw error.response?.data || new Error("Failed to create user");
  }
};


// Update user
export const updateUser = async (userId, data, token) => {
    console.log(data)

  try {
    const res = await axiosInstance.put(API_PATHS.UPDATE_USER(userId), data, {
      headers: {
        Authorization: `Bearer ${token}`,
      },
    });
    return res.data;
  } catch (error) {
    console.error("Failed to update user:", error);
    throw new Error("Failed to update user.");
  }
};

// Updated Send OTP for Forgot Password
export const requestForgotOtp = (loginId, setMessage, setOtpSent, setTimer) => {
  console.log("loginid"+loginId)
  axiosInstance
    .post(API_PATHS.REQUEST_OTP, {
      username: loginId,
      otp_type: "Forgot",
    })
    .then(() => {
      showAlert("OTP has been sent successfully!", "success");
      setTimeout(() => setMessage(""), 3000);
      setOtpSent(true);
      setTimer(120);
    })
    .catch(() => {
      showAlert("Failed to send OTP. Please check the User ID.", "error");
    });
};

// Updated Verify OTP for Forgot Password
export const verifyForgotOtp = (loginId, otp, setMessage, navigate) => {
  axiosInstance
    .post(API_PATHS.VERIFY_OTP, {
      username: loginId,
      otp: otp,
    })
    .then((res) => {
      const token = res.data.data.token;
      console.log(token)
      localStorage.setItem("resetToken", token);
      navigate("/home/reset");
      showAlert("OTP has been verified successfully!", "success");
    })
    .catch(() => {
      showAlert("Invalid OTP or User ID. Please try again.", "error");
    });
};

// forgot  password
export const resetPassword = async (loginId, newPassword, token) => {
  console.log(token)
  try {
    const response = await axiosInstance.post(API_PATHS.RESET_PASSWORD,  //  Corrected path
      {
        username: loginId,             //  Match cURL: use "username"
        password: newPassword,
      },
      {
        headers: {
          Authorization: `Bearer ${token}`,
        },
      }
    );
    return response.data;
  } catch (error) {
    console.error("Failed to reset password:", error);
    throw new Error(error.response?.data?.message || "Password reset failed.");
  }
};


// change password
export const changePassword = async (username, old_password, newPassword, token) => {
  const response = await axiosInstance.post(
    API_PATHS.CHANGE_PASSWORD,
    {
      username,
      old_password: old_password,
      new_password: newPassword,
    },
    {
      headers: {
        Authorization: `Bearer ${token}`,
      },
    }
  );
  return response.data;
};


// Activate user 

export const activateUser = async (userId, token) => {
  return await axiosInstance.get(
    API_PATHS.ACTIVATE_USER(userId),

    {
      headers: {
        Authorization: `Bearer ${token}`,
      },
    }
  );
};

// De-activate user
export const deactivateUser = async (userId, token) => {
  return await axiosInstance.get(
    API_PATHS.DEACTIVATE_USER(userId),
    
    {
      headers: {
        Authorization: `Bearer ${token}`,
      },
    }
  );
};

//  Resgister uri with roles
export const registerUriWithRoles = async (payload, token) => {
  console.log(token)
  try {
    const response = await axiosInstance.post(
      "/object/register",
      payload,
      {
        headers: {
          "Content-Type": "application/json",
          Authorization: `Bearer ${token}`,
        },
      }
    );
    return response.data; // return data on success
  } catch (error) {
    // Throw error message for caller to handle
    throw error.response?.data?.message || error.message || "Failed to register object";
  }
};

// Assign roles to an existing URI
export const assignRolesToUri = async (payload, token) => {

  const res= await axiosInstance.post("/object/assign-role", payload, {
    headers: {
      Authorization: `Bearer ${token}`,
    },
  });
  return res.data;
};


export const deassignRolesFromUri = async (payload, token) => {
  const res= await axiosInstance.post("/object/de-assign-role", payload, {
    headers: {
      Authorization: `Bearer ${token}`,
    },
  });
  console.log(res.data)
  return res.data;

};

// GET /dms/object/ - Fetch all objects
// export const fetchObjects = async (setObjects,token) => {
//   try {
//     const response = await axiosInstance.get('/object/', {
//       headers: {
//         Authorization: `Bearer ${token}`,
//       },
//     });
//     console.log(response.data.data.content)
//     setObjects(response.data.data.content);
//     return response; // Assuming the data is a list of ObjectMaster DTOs
//   } catch (error) {
//     console.error('Error fetching objects:', error);
//     throw error; // So the caller can handle it (toast, fallback UI, etc.)
//   }
// };


//   FETCH ALL OBJECT-URI 
export const fetchPaginatedObjects = async (pageNumber, pageSize, token) => {
  const response = await axiosInstance.get(
    `/object/?pageNumber=${pageNumber}&pageSize=${pageSize}`,
    {
      headers: {
        Authorization: `Bearer ${token}`,
      },
    }
  );
  return response.data.data; // access the actual paginated content
};


// ENABLE OBJECT-URI
export const enableObjectUri = async (id, token) => {
  try {
    const response = await axiosInstance.get(
      `/object/enable/${id}`,
      {
        headers: {
          Authorization: `Bearer ${token}`,
        },
      }
    );
    return response.data; // e.g., "Object with ID 5 has been enabled."
  } catch (error) {
    console.error(`Failed to enable object with ID ${id}:`, error);
    throw error;
  }
};

// DISABLE OBJECT-URI
export const disableObjectUri = async (id, token) => {
  try {
    const response = await axiosInstance.get(
      `/object/disable/${id}`,
      {
        headers: {
          Authorization: `Bearer ${token}`,
        },
      }
    );
    return response.data; // e.g., "Object with ID 5 has been disabled."
  } catch (error) {
    console.error(`Failed to disable object with ID ${id}:`, error);
    throw error;
  }
};

export { axiosInstance }; // Named export
