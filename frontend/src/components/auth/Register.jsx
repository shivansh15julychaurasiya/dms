import React, { useState } from "react";
import { useNavigate } from "react-router-dom";
import { saveUser } from "../../services/axios";

import {
  Container,
  Row,
  Col,
  Card,
  CardBody,
  Form,
  FormGroup,
  Label,
  Input,
  Button,
  Alert,
} from "reactstrap";
import "../../assets/styles.css"

const Register = () => {
  const navigate = useNavigate();
  const [formData, setFormData] = useState({
    name: "",
    email: "",
    about: "",
    password: "",
    phone: "",
    login_id: "",
  });
  const [error, setError] = useState("");

  const handleChange = (e) => {
    setFormData((prev) => ({
      ...prev,
      [e.target.name]: e.target.value,
    }));
  };

  const handleSubmit = async (e) => {
    e.preventDefault();

    const { name, email, password, phone, about, login_id } = formData;

    if (!name || !email || !password || !phone || !about || !login_id) {
      setError("All fields are required.");
      return;
    }

    const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
    if (!emailRegex.test(email)) {
      setError("Invalid email format.");
      return;
    }

    if (password.length < 3) {
      setError("Password must be at least 3 characters.");
      return;
    }

    setError("");

    try {
      await saveUser(formData, navigate);
    } catch (err) {
      setError(err.message);
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
    maxWidth: "550px",
    width: "100%",
    boxShadow: "0 4px 20px rgba(0, 0, 0, 0.3)",
  };

  return (
    <div className="registerddd-background" style={wrapperStyle}>
      <Container className="d-flex justify-content-center align-items-center min-vh-100">
        <Card style={cardStyle}>
          <CardBody>
            <h3 className="text-center text-primary mb-4">Create User</h3>
            {error && <Alert color="danger">{error}</Alert>}
            <Form onSubmit={handleSubmit}>
              <Row>
                <Col md={6}>
                  <FormGroup>
                    <Label for="name">Name</Label>
                    <Input
                      type="text"
                      name="name"
                      value={formData.name}
                      onChange={handleChange}
                      
                    />
                  </FormGroup>
                </Col>
                <Col md={6}>
                  <FormGroup>
                    <Label for="email">Email</Label>
                    <Input
                      type="email"
                      name="email"
                      value={formData.email}
                      onChange={handleChange}
                      
                    />
                  </FormGroup>
                </Col>
              </Row>

              <Row>
                <Col md={6}>
                  <FormGroup>
                    <Label for="phone">Phone</Label>
                    <Input
                      type="text"
                      name="phone"
                      value={formData.phone}
                      onChange={handleChange}
                      
                    />
                  </FormGroup>
                </Col>
                <Col md={6}>
                  <FormGroup>
                    <Label for="login_id">Employee ID (Login ID)</Label>
                    <Input
                      type="text"
                      name="login_id"
                      value={formData.login_id}
                      onChange={handleChange}
                      
                    />
                  </FormGroup>
                </Col>
              </Row>

              <Row>
                <Col md={6}>
                  <FormGroup>
                    <Label for="password">Password</Label>
                    <Input
                      type="password"
                      name="password"
                      value={formData.password}
                      onChange={handleChange}
                      
                    />
                  </FormGroup>
                </Col>
                <Col md={6}>
                  <FormGroup>
                    <Label for="about">About</Label>
                    <Input
                      type="text"
                      name="about"
                      value={formData.about}
                      onChange={handleChange}
                      
                    />
                  </FormGroup>
                </Col>
              </Row>

              <div className="text-center mt-3">
                <Button color="primary" type="submit">
                  Register
                </Button>
              </div>
            </Form>
          </CardBody>
        </Card>
      </Container>
    </div>
  );
};

export default Register;
