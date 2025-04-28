import React, { useContext, useState } from "react";
import { Link, useNavigate } from "react-router-dom";
import {
  Container, Row, Col, Card, CardBody, Form, FormGroup, Label, Input, Button, Alert,
} from "reactstrap";
import { loginUser } from "../../services/axios";
import { getRoleRedirectPath, showAlert } from "../../utils/helpers";
import { AuthContext } from "../../context/AuthContext";

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
      console.log("role: " + user.roles[0].role_name.trim());

      navigate(getRoleRedirectPath( user.roles[0].role_name.trim()));
    } catch {
      showAlert("!!Invalid User ID or Password!!")
      // setErrorMsg("Invalid User ID or Password.");
    }
  };

  return (
    <Container fluid className="min-vh-100 d-flex justify-content-center align-items-center bg-light register-background">
      <Row className="w-100 justify-content-center">
        <Col md={5} lg={4}>
          <Card className="shadow-lg border-0 cardStyle">
            <CardBody>
              <h2 className="text-center text-primary fw-bold mb-4">
                <i className="bi bi-person-circle me-2"></i>Login
              </h2>
              {errorMsg && <Alert color="danger">{errorMsg}</Alert>}
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
                  />
                </FormGroup>
                <div className="d-grid">
                  <Button color="primary" type="submit">
                    <i className="bi bi-box-arrow-in-right me-1"></i> Login
                  </Button>
                </div>
                <div className="text-center mt-3">
                  <Link to="/home/forgot" className="text-danger fw-bold">Forgot Password?</Link>
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
