import API from "./AuthService";



/** Create Product */
export const createProduct = (token, data) => {
  const formData = new FormData();
  formData.append("name", data.name);
  formData.append("description", data.description);
  formData.append("price", data.price);
  formData.append("stock", data.stock);

  if (data.categoryId) formData.append("categoryId", data.categoryId);
  if (data.subCategoryId) formData.append("subCategoryId", data.subCategoryId);
  if (data.image) formData.append("image", data.image);

  return API.post(API.BASE_URL, formData, {
    headers: {
      Authorization: `Bearer ${token}`,
      "Content-Type": "multipart/form-data",
    },
  });
};

/** Get all products (pagination supported) */
export const getProducts = (token, page = 0, size = 10) => {
  return API.get(`${API.BASE_URL}?page=${page}&size=${size}`, {
    headers: { Authorization: `Bearer ${token}` },
  });
};

/** Delete product */
export const deleteProduct = (token, id) => {
  return API.delete(`${API.BASE_URL}/${id}`, {
    headers: { Authorization: `Bearer ${token}` },
  });
};

/** Update product */
export const updateProduct = (token, id, data) => {
  const formData = new FormData();
  formData.append("name", data.name);
  formData.append("description", data.description);
  formData.append("price", data.price);
  formData.append("stock", data.stock);

  if (data.categoryId) formData.append("categoryId", data.categoryId);
  if (data.subCategoryId) formData.append("subCategoryId", data.subCategoryId);
  if (data.image) formData.append("image", data.image);

  return API.put(`${API.BASE_URL}/${id}`, formData, {
    headers: {
      Authorization: `Bearer ${token}`,
      "Content-Type": "multipart/form-data",
    },
  });
};
