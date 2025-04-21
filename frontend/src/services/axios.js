import axios from "axios";

const loginUser = async (loginId, password) => {
    try {
      const response = await fetch("http://localhost:8081/dms/auth/login-password", {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
        },
        credentials: "include",
        body: JSON.stringify({
          username: loginId,
          password: password,
        }),
      });
  
      if (!response.ok) {
        throw new Error("Login failed");
      }
  
      const data = await response.json();
      return data;
    } catch (err) {
      throw new Error(err.message);
    }
  };


const apiUrl = "http://localhost:8081/dms";

// Function to check if the token is expired
export const isTokenExpired = (token) => {
  const decodedToken = JSON.parse(atob(token.split('.')[1]));
  const currentTime = Math.floor(Date.now() / 1000);
  return decodedToken.exp < currentTime;
};

// Function to fetch users
export const fetchUsers = async (setUsers, setError, navigate) => {
  try {
    const token = localStorage.getItem("token");
    if (!token || isTokenExpired(token)) {
      localStorage.removeItem("token");
      navigate("/home/login");
      return;
    }

    const res = await axios.get(`${apiUrl}/users/`, {
      headers: {
        Authorization: `Bearer ${token}`,
      },
    });

    setUsers(res.data);
  } catch (err) {
    console.error(err.message);
    setError("Error loading users. You may not be authorized.");
  }
};

// Function to delete user
export const deleteUser = async (userId, users, setUsers, setError, navigate) => {
  if (!window.confirm("Are you sure you want to delete this user?")) return;

  try {
    const res = await axios.delete(`${apiUrl}/users/${userId}`, {
      headers: {
        Authorization: `Bearer ${localStorage.getItem("token")}`,
      },
    });

    setUsers(users.filter((user) => user.userId !== userId));
  } catch (err) {
    console.error(err.message);
    alert("Could not delete user.");
  }
};

// Function to save user (create or edit)
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
      ? `${apiUrl}/users/${selectedUser.userId}`
      : `${apiUrl}/auth/register`;

  const method = modalMode === "edit" ? "put" : "post";

  try {
    const res = await axios({
      method,
      url,
      data: selectedUser,
      headers: {
        "Content-Type": "application/json",
        ...(modalMode === "edit" && { Authorization: `Bearer ${token}` }),
      },
    });

    fetchUsers(setUsers, setError); // Re-fetch users after save
    setShowModal(false);
  } catch (err) {
    console.error(err.message);
    alert("Failed to save user.");
  }
};

  
  export default loginUser;
  