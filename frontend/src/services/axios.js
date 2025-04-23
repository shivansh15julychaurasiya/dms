import axios from "axios";

const apiUrl = "http://localhost:8081/dms"; // Base API URL

// Create an axios instance for API requests
const axiosInstance = axios.create({
  baseURL: apiUrl,
  headers: {
    "Content-Type": "application/json",
  },
});

// Login function
export const loginUser = async (loginId, password, setErrorMsg, navigate) => {
  const allowedRoles = ["ROLE_ADMIN", "ROLE_USER", "ROLE_ECOURT"];

  try {
    const response = await axiosInstance.post("/auth/login-password", {
      username: loginId,
      password: password,
    });

    const { token, user } = response.data.data;
    const role = user.roles[0].name.trim(); // Ensure role is correctly trimmed

    localStorage.setItem("token", token);
    localStorage.setItem("user", JSON.stringify(user));
    localStorage.setItem("role",role);

    // Check if the role is allowed
    if (!allowedRoles.includes(role)) {
      navigate("/home/unauthorize"); // Redirect to unauthorized route
    } else {
      // Redirect based on role
      switch (role) {
        case "ROLE_ADMIN":
          navigate("/home/admindashboard"); // Admin dashboard path
          break;
        case "ROLE_USER":
          navigate("/home/userdashboard"); // User dashboard path
          break;
        case "ROLE_MANAGER":
          navigate("/home/managerdashboard"); // Manager dashboard path
          break;
        default:
          navigate("/home/unauthorize"); // Default to unauthorized route
      }
    }

  } catch (error) {
    console.error("Login error:", error);
    setErrorMsg("Invalid email or password");
    alert("Login failed.");
  }
};

// Token expiry checker
export const isTokenExpired = (token) => {
  try {
    const decodedToken = JSON.parse(atob(token.split(".")[1]));
    const currentTime = Math.floor(Date.now() / 1000);
    return decodedToken.exp < currentTime;
  } catch (error) {
    console.error("Token decode error:", error);
    return true;
  }
};

// Fetch all users
export const fetchUsers = async (setUsers, setError, navigate) => {
  try {
    const token = localStorage.getItem("token");
    if (!token || isTokenExpired(token)) {
      localStorage.removeItem("token");
      navigate("/home/login");
      return;
    }

    const res = await axiosInstance.get("/users/", {
      headers: {
        Authorization: `Bearer ${token}`,
      },
    });

    setUsers(res.data);
  } catch (err) {
    console.error("Fetch users error:", err);
    setError("Error loading users. You may not be authorized.");
  }
};

// Delete a user
export const deleteUser = async (userId, users, setUsers, setError, navigate) => {
  if (!window.confirm("Are you sure you want to delete this user?")) return;

  try {
    await axiosInstance.delete(`/users/${userId}`, {
      headers: {
        Authorization: `Bearer ${localStorage.getItem("token")}`,
      },
    });

    setUsers(users.filter((user) => user.userId !== userId));
  } catch (err) {
    console.error("Delete user error:", err);
    alert("Could not delete user.");
  }
};

// Create or update a user
export const handleFormSubmit = async (
  e,
  selectedUser,
  modalMode,
  users,
  setUsers,
  setError,
  setShowModal
) => {
  e.preventDefault();

  const token = localStorage.getItem("token");

  const url =
    modalMode === "edit"
      ? `/users/${selectedUser.userId}`
      : "/auth/register";  // Use relative paths based on the base URL

  const method = modalMode === "edit" ? "put" : "post";

  try {
    await axiosInstance({
      method,
      url,
      data: selectedUser,
      headers: {
        "Content-Type": "application/json",
        ...(modalMode === "edit" && { Authorization: `Bearer ${token}` }),
      },
    });

    await fetchUsers(setUsers, setError, () => {});
    setShowModal(false);
  } catch (err) {
    console.error("Save user error:", err);
    alert("Failed to save user.");
  }
};
