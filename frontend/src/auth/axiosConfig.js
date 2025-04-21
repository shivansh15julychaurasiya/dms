// src/api/axiosConfig.js
import axios from "axios";
import { isTokenExpired, getToken } from "./authUtils";

const api = axios.create({
  baseURL: "http://localhost:8081", // change to your backend URL
});

api.interceptors.request.use(
  (config) => {
    const token = getToken();

    if (token && isTokenExpired(token)) {
      localStorage.removeItem("token");
      window.location.href = "/login";
      return Promise.reject("Token expired. Redirecting to login.");
    }

    if (token) {
      config.headers.Authorization = `Bearer ${token}`;
    }

    return config;
  },
  (error) => Promise.reject(error)
);

export default api;
