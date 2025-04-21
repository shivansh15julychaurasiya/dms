// src/auth/authUtils.js
export const isTokenExpired = (token) => {
    if (!token) return true;
  
    try {
      const payload = JSON.parse(atob(token.split('.')[1]));
      return payload.exp * 1000 < Date.now();
    } catch (error) {
      return true; // treat errors as expired
    }
  };
  console.log(localStorage.getItem("token"))
  export const getToken = () => localStorage.getItem("token");
  