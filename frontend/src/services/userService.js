// src/services/UserService.js
import API from "./AuthService";

//  1. Get All Users
export const getAllUsers = async () => {
  const res = await API.get("/dms/users/");
  return res.data;
};

//  2. Create User
export const createUser = async (userData) => {
  const res = await API.post("/dms/users/", userData);
  return res.data;
};

//  3. Update User
export const updateUser = async (userId, userData) => {
  const res = await API.put(`/dms/users/${userId}`, userData);
  return res.data;
};

//  4. Delete User
export const deleteUser = async (userId) => {
  const res = await API.delete(`/dms/users/${userId}`);
  return res.data;
};

//  5. Get All Roles (for dropdown)
export const getRoles = async () => {
  const res = await API.get("/dms/roles/");
  return res.data;
};

// ✅ 6. Get Single User Details
export const getUserById = async (userId) => {
  const res = await API.get(`/dms/users/${userId}`);
  return res.data;
};

// ✅ 7. User Activity Log (Side Drawer)
export const getUserActivity = async (userId) => {
  const res = await API.get(`/dms/users/${userId}/activity`);
  return res.data;
};
