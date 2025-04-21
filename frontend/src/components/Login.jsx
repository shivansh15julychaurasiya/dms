import React, { useState } from "react";
import "bootstrap/dist/css/bootstrap.min.css";
import "bootstrap-icons/font/bootstrap-icons.css";
import { Link, useNavigate } from "react-router-dom";

const Login = () => {
  const [loginId, setLoginId] = useState(""); // backend expects loginId as username
  const [password, setPassword] = useState("");
  const [errorMsg, setErrorMsg] = useState("");
  const navigate = useNavigate();

  const handleLogin = async (e) => {
    e.preventDefault();

    try {
      const res = await fetch("http://localhost:8081/dms/auth/login-password", {
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

      if (!res.ok) {
        throw new Error("Login failed");
      }

      const data = await res.json();
      localStorage.setItem("token", data.data.token);

      const token = localStorage.getItem("token");
      console.log(token)


      // Optional: redirect to dashboard
      navigate("/home/userdashboard");
    } catch (err) {
      console.error(err.message);
      setErrorMsg("Invalid credentials. Please try again.");
    }
  };

  const wrapperStyle = {
    height: "100vh",
    backgroundImage:
      "url('https://as2.ftcdn.net/jpg/06/12/69/39/1000_F_612693965_Ic0XfvkMa44xQXHA8lonULgqoEzyS0Xl.jpg')",
    backgroundSize: "cover",
    backgroundPosition: "center",
    display: "flex",
    alignItems: "center",
    justifyContent: "center",
  };

  const cardStyle = {
    backgroundColor: "rgba(255, 255, 255, 0.95)",
    padding: "2rem",
    borderRadius: "2rem",
    maxWidth: "410px",
    width: "100%",
    boxShadow: "0 4px 20px rgba(0, 0, 0, 0.3)",
  };

  return (
    <div style={wrapperStyle}>
      <div style={cardStyle}>
        <h2 className="text-center mb-4 fw-bold shimmer-text">
          <i className="bi bi-person-circle me-2 "></i>Login
        </h2>
        {errorMsg && (
          <div className="alert alert-danger py-1 text-center">{errorMsg}</div>
        )}
        <form onSubmit={handleLogin}>
          <div className="mb-3">
            <label htmlFor="loginId" className="form-label fw-bold shimmer-text ">
              User ID:
            </label>
            <input
              type="number"
              className="form-control "
              id="loginId"
              placeholder="Enter User ID"
              value={loginId}
              onChange={(e) => setLoginId(e.target.value)}
              required
            />
          </div>
          <div className="mb-3">
            <label htmlFor="password" className="form-label fw-bold shimmer-text ">
              Password:
            </label>
            <input
              type="password"
              className="form-control"
              id="password"
              placeholder="Enter password"
              value={password}
              onChange={(e) => setPassword(e.target.value)}
              required
            />
          </div>
          <div className="d-grid">
            <button
              type="submit"
              className="btn btn-primary login-btn "
            >
              <i className="bi bi-box-arrow-in-right me-1 "></i> Login
            </button>
          </div>
         
          <div className="text-center mt-3">
            <Link to="/home/forgot" className=" text-danger fw-bold shimmer-text">
              Forgot Password?
            </Link>
          </div>
          {/* <div className="text-center mt-2 text-dark">
            Don't have an account?{" "}
            <Link to="/home/register" className="text-decoration-none fw-bold">
              Register
            </Link>
          </div> */}
        </form>
      </div>
    </div>
  );
};

export default Login;
