import API from "./AuthService";



/** Add SubCategory */
export const addSubCategory = (token, data) => {
  return API.post(API.BASE_URL, data, {
    headers: { Authorization: `Bearer ${token}` },
  });
};

/** Get All SubCategories */
export const getAllSubCategories = (token) => {
  return API.get(API.BASE_URL, {
    headers: { Authorization: `Bearer ${token}` },
  });
};

/** Get SubCategories by Category ID */
export const getByCategoryId = (token, categoryId) => {
  return API.get(`${API.BASE_URL}/category/${categoryId}`, {
    headers: { Authorization: `Bearer ${token}` },
  });
};

/** Update SubCategory */
export const updateSubCategory = (token, id, data) => {
  return API.put(`${API.BASE_URL}/${id}`, data, {
    headers: { Authorization: `Bearer ${token}` },
  });
};

/** Delete SubCategory */
export const deleteSubCategory = (token, id) => {
  return API.delete(`${API.BASE_URL}/${id}`, {
    headers: { Authorization: `Bearer ${token}` },
  });
};
