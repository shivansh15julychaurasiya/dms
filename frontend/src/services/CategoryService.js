// src/services/CategoryService.js
import API from "./AuthService";

/** 1. Get All Categories */
export const getAllCategories = async () => {
  const res = await API.get("/grocify/categories");
  return res.data;
};

/** 2. Create Category */
export const createCategory = async (data) => {
  const res = await API.post("/grocify/categories", data);
  return res.data;
};

/** 3. Delete Category */
export const deleteCategory = async (id) => {
  const res = await API.delete(`/grocify/categories/${id}`);
  return res.data;
};
