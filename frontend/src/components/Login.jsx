import React, { useState } from "react";
import { Container, Card, CardBody, Form, FormGroup, Label, Input, Button, Alert } from "reactstrap";
import { Link, useNavigate } from "react-router-dom"; // Import Link from react-router-dom
import "bootstrap/dist/css/bootstrap.min.css"; // Make sure Bootstrap is imported
import "bootstrap-icons/font/bootstrap-icons.css"; // Bootstrap icons for styling

const Login = () => {
  const [loginId, setLoginId] = useState("");
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

      console.log("Logged in user:", data.data.user);

      // Store user data in localStorage (if needed)
      localStorage.setItem("user", JSON.stringify(data.data.user));

      // Redirect to /home/userdashboard
      navigate("/home/userdashboard");
    } catch (err) {
      console.error("Login error:", err.message);
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
      <Card style={cardStyle}>
        <h2 className="text-center mb-4 fw-bold shimmer-text">
          <i className="bi bi-person-circle me-2"></i>Login
        </h2>
        {errorMsg && <Alert color="danger" className="text-center">{errorMsg}</Alert>}
        <CardBody>
          <Form onSubmit={handleLogin}>
            <FormGroup>
              <Label for="loginId" className="fw-bold shimmer-text">User ID:</Label>
              <Input
                type="number"
                id="loginId"
                placeholder="Enter User ID"
                value={loginId}
                onChange={(e) => setLoginId(e.target.value)}
                required
              />
            </FormGroup>
            <FormGroup>
              <Label for="password" className="fw-bold shimmer-text">Password:</Label>
              <Input
                type="password"
                id="password"
                placeholder="Enter password"
                value={password}
                onChange={(e) => setPassword(e.target.value)}
                required
              />
            </FormGroup>
            <Button color="primary" block type="submit">
              <i className="bi bi-box-arrow-in-right me-1"></i> Login
            </Button>
            <div className="text-center mt-3">
              <Link to="/home/forgot" className="text-danger fw-bold shimmer-text">
                Forgot Password?
              </Link>
            </div>
            {/* Uncomment this if registration is needed */}
            {/* <div className="text-center mt-2 text-dark">
              Don't have an account?{" "}
              <Link to="/home/register" className="text-decoration-none fw-bold">
                Register
              </Link>
            </div> */}
          </Form>
        </CardBody>
      </Card>
    </div>
  );
};

export default Login;
