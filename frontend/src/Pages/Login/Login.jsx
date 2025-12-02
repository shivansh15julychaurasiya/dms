import React, { useState } from "react";
import {
  Button,
  Form,
  FormGroup,
  Input,
  Label,
  Card,
  CardBody,
} from "reactstrap";
import { useNavigate } from "react-router-dom";
import { loginApi } from "../../services/AuthService";

import Snackbar from "@mui/material/Snackbar";
import MuiAlert from "@mui/material/Alert";
import { useAuth } from "../../context/AuthContext";

export default function Login() {
  const navigate = useNavigate();
  const { login } = useAuth();

  const [formData, setFormData] = useState({
    username: "",
    password: "",
  });

  const [loading, setLoading] = useState(false);

  // Snackbar
  const [error, setError] = useState("");
  const [openSnack, setOpenSnack] = useState(false);

  const handleSnackClose = () => setOpenSnack(false);

  const showError = (msg) => {
    setError(msg);
    setOpenSnack(true);
  };

  const handleChange = (e) => {
    setFormData({ ...formData, [e.target.name]: e.target.value });
  };

  const validate = () => {
    const { username, password } = formData;

    if (!username || !password) {
      showError("Username and Password are required.");
      return false;
    }
    if (password.length < 4) {
      showError("Password must be at least 4 characters.");
      return false;
    }
    return true;
  };

  const handleSubmit = async (e) => {
    e.preventDefault();

    if (!validate()) return;
    setLoading(true);

    try {
      const response = await loginApi(formData.username, formData.password);

      if (response.data.status === false) {
        showError(response.data.message);
        return;
      }

      console.log("response data:", response.data);

      const token = response.data.data.token;
      const userData = response.data.data.user;

      if (!token) {
        showError("No token received from server");
        return;
      }

      // Save access token
      localStorage.setItem("accessToken", token);

      // Update AuthContext
      login(token, userData);

      // Redirect to dashboard
      navigate("/dashboard", { replace: true });

    } catch (err) {
      console.error("Login error:", err);
      showError(err.response?.data?.message || "Invalid username or password");
    } finally {
      setLoading(false);
    }
  };

  return (
    <div className="login-bg d-flex justify-content-center align-items-center">
      <Card className="card-custom p-4" style={{ width: "400px" }}>
        <CardBody>
          <h3 className="text-center mb-4">Login</h3>

          <Form onSubmit={handleSubmit}>
            <FormGroup>
              <Label>Username:</Label>
              <Input
                type="text"
                name="username"
                value={formData.username}
                onChange={handleChange}
                placeholder="Enter username"
              />
            </FormGroup>

            <FormGroup>
              <Label>Password:</Label>
              <Input
                type="password"
                name="password"
                value={formData.password}
                onChange={handleChange}
                placeholder="Enter password"
              />
            </FormGroup>

            <Button color="primary" block className="mt-3" disabled={loading}>
              {loading ? "Logging in..." : "Login"}
            </Button>
          </Form>
        </CardBody>
      </Card>

      {/* Error Snackbar */}
      <Snackbar
        open={openSnack}
        autoHideDuration={4000}
        onClose={handleSnackClose}
        anchorOrigin={{ vertical: "top", horizontal: "center" }}
      >
        <MuiAlert
          onClose={handleSnackClose}
          severity="error"
          variant="filled"
          elevation={6}
          sx={{ width: "100%" }}
        >
          {error}
        </MuiAlert>
      </Snackbar>
    </div>
  );
}
