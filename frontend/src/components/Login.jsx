import React, { useState } from "react";
import {
  Card,
  CardBody,
  Form,
  FormGroup,
  Label,
  Input,
  Button,
  Alert,
} from "reactstrap";
import { Link, useNavigate } from "react-router-dom";
import "bootstrap/dist/css/bootstrap.min.css";
import "bootstrap-icons/font/bootstrap-icons.css";
import loginUser from "../services/axios";

const Login = () => {
  const [formData, setFormData] = useState({ loginId: "", password: "" });
  const [errorMsg, setErrorMsg] = useState("");
  const navigate = useNavigate();

  const handleChange = (e) => {
    setFormData((prev) => ({
      ...prev,
      [e.target.name]: e.target.value,
    }));
  };

  const handleLogin = async (e) => {
    e.preventDefault();
    setErrorMsg("");

    try {
      const { data } = await loginUser(formData.loginId, formData.password);
      localStorage.setItem("token", data.token);
      localStorage.setItem("user", JSON.stringify(data.user));
      navigate("/home/userdashboard");
    } catch (err) {
      console.error("Login error:", err.message);
      setErrorMsg("Invalid credentials. Please try again.");
    }
  };

  const styles = {
    wrapper: {
      height: "100vh",
      backgroundImage:
        "url('https://as2.ftcdn.net/jpg/06/12/69/39/1000_F_612693965_Ic0XfvkMa44xQXHA8lonULgqoEzyS0Xl.jpg')",
      backgroundSize: "cover",
      backgroundPosition: "center",
      display: "flex",
      alignItems: "center",
      justifyContent: "center",
    },
    card: {
      backgroundColor: "rgba(255, 255, 255, 0.95)",
      padding: "2rem",
      borderRadius: "2rem",
      maxWidth: "410px",
      width: "100%",
      boxShadow: "0 4px 20px rgba(0, 0, 0, 0.3)",
    },
  };

  return (
    <div style={styles.wrapper}>
      <Card style={styles.card}>
        <h2 className="text-center mb-4 fw-bold shimmer-text">
          <i className="bi bi-person-circle me-2"></i>Login
        </h2>

        {errorMsg && (
          <Alert color="danger" className="text-center">
            {errorMsg}
          </Alert>
        )}

        <CardBody>
          <Form onSubmit={handleLogin}>
            <FormGroup>
              <Label for="loginId" className="fw-bold shimmer-text">
                User ID:
              </Label>
              <Input
                type="number"
                name="loginId"
                id="loginId"
                placeholder="Enter User ID"
                value={formData.loginId}
                onChange={handleChange}
                required
              />
            </FormGroup>

            <FormGroup>
              <Label for="password" className="fw-bold shimmer-text">
                Password:
              </Label>
              <Input
                type="password"
                name="password"
                id="password"
                placeholder="Enter password"
                value={formData.password}
                onChange={handleChange}
                required
              />
            </FormGroup>

            <Button color="primary" block type="submit">
              <i className="bi bi-box-arrow-in-right me-1"></i> Login
            </Button>

            <div className="text-center mt-3">
              <Link
                to="/home/forgot"
                className="text-danger fw-bold shimmer-text"
              >
                Forgot Password?
              </Link>
            </div>
          </Form>
        </CardBody>
      </Card>
    </div>
  );
};

export default Login;
