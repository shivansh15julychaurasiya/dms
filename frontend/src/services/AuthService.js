import axios from "axios";

// Create instance
const API = axios.create({
  baseURL: "http://localhost:8081",
});

// Add Access Token on every request
API.interceptors.request.use((config) => {
  const accessToken = localStorage.getItem("accessToken");

  if (accessToken) {
    config.headers.Authorization = `Bearer ${accessToken}`;
  }

  return config;
});

// Refresh token interceptor
API.interceptors.response.use(
  (res) => res,
  async (err) => {
    const originalRequest = err.config;

    // If token expired → try refresh token
    if (err.response?.status === 401 && !originalRequest._retry) {
      originalRequest._retry = true;

      try {
        const refreshToken = localStorage.getItem("refreshToken");

        if (!refreshToken) {
          logoutUser();
          return Promise.reject(err);
        }

        // Call refresh endpoint
        const refreshResponse = await axios.post(
          "http://localhost:8081/dms/auth/refresh-token",
          { refreshToken }
        );

        // Save new tokens
        localStorage.setItem("accessToken", refreshResponse.data.accessToken);
        localStorage.setItem("refreshToken", refreshResponse.data.refreshToken);

        // Retry original request
        originalRequest.headers.Authorization =
          `Bearer ${refreshResponse.data.accessToken}`;

        return API(originalRequest);
      } catch (refreshError) {
        logoutUser();
        return Promise.reject(refreshError);
      }
    }

    return Promise.reject(err);
  }
);

// Clear tokens & redirect to login
function logoutUser() {
  localStorage.removeItem("accessToken");
  localStorage.removeItem("refreshToken");
  window.location.href = "/";
}

// LOGIN API
export const loginApi = (username, password) => {
  return API.post("/dms/auth/login-password", {
    username,
    password,
  });
};

// Export instance
export default API;
