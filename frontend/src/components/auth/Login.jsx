import React, { useContext, useState } from "react";
import { Link, useNavigate } from "react-router-dom";
import {
  Container, Row, Col, Card, CardBody, Form, FormGroup, Label, Input, Button, Alert,
} from "reactstrap";
import { loginUser } from "../../services/userService";
import { getRoleRedirectPath, showAlert } from "../../utils/helpers";
import { AuthContext } from "../../context/AuthContext";
// import "../../assets/styles.css"

const Login = () => {
  const { login } = useContext(AuthContext);
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
      const { token, user } = await loginUser(formData.loginId, formData.password);
      login({ token, user });

      const roleNames = user.roles.map(role => role.role_name);
      console.log(roleNames);
      // console.log("role: " + user.roles.role_name.trim());
      showAlert("Login successfully" ,"success")
      navigate(getRoleRedirectPath(roleNames));
    } catch {
      showAlert("!!Invalid User ID or Password!!");
    }
  };

  return (
    
    <Container fluid className="min-vh-100 d-flex justify-content-center align-items-center   register-background ">
      <Row className="w-100 justify-content-center ">
        <Col md={6} lg={4}>
          <Card className="shadow-lg border-1 rounded-4 cardStyle " >
            <CardBody>
              <h2 className="text-center text-primary fw-bold mb-4">
                <i className="bi bi-person-circle me-2"></i>Login
              </h2>
              {errorMsg && <Alert color="danger" className="fade-in">{errorMsg}</Alert>}
              <Form onSubmit={handleLogin}>
                <FormGroup>
                  <Label for="loginId" className="fw-bold text-primary">User ID:</Label>
                  <Input
                    type="number"
                    id="loginId"
                    name="loginId"
                    value={formData.loginId}
                    onChange={handleChange}
                    required
                    className="rounded-3 border-2"
                    placeholder="Enter your User ID"
                  />
                </FormGroup>
                <FormGroup>
                  <Label for="password" className="fw-bold text-primary">Password:</Label>
                  <Input
                    type="password"
                    id="password"
                    name="password"
                    value={formData.password}
                    onChange={handleChange}
                    required
                    className="rounded-3 border-2"
                    placeholder="Enter your password"
                  />
                </FormGroup>
                <div className="d-grid">
                  <Button color="primary" type="submit" className="py-2 fs-5 rounded-pill hover-shadow">
                    <i className="bi bi-box-arrow-in-right me-1"></i> Login
                  </Button>
                </div>
                <div className="text-center mt-3">
                  <Link to="/home/forgot" className="text-danger fw-bold text-decoration-none">
                    Forgot Password?
                  </Link>
                </div>
              </Form>
            </CardBody>
          </Card>
        </Col>
      </Row>
    </Container>
  );
};

export default Login;
