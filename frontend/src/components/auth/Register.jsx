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

import "../../assets/styles.css";

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

  const backToDashboard = () => {
    window.location.href = "/dms/home/admindashboard";
  };

  return (
    <div className="register-background wrapperStyle" style={{ minHeight: "100vh" }}>
      <Container fluid className="d-flex justify-content-center align-items-center py-4 px-2">
        <Col xs={12} sm={11} md={8} lg={6} xl={5}>
          <Card className="cardStyle shadow" style={{ maxHeight: "90vh", overflowY: "auto" }}>
            <CardBody className="p-4">
              <h3 className="text-center text-primary mb-4 shimmer-text">Create User</h3>
              {error && <Alert color="danger">{error}</Alert>}
              <Form onSubmit={handleSubmit}>
                <Row>
                  <Col xs={12} md={6} className="mb-3">
                    <FormGroup>
                      <Label for="name">Name</Label>
                      <Input
                        type="text"
                        name="name"
                        value={formData.name}
                        onChange={handleChange}
                        placeholder="Enter full name"
                      />
                    </FormGroup>
                  </Col>
                  <Col xs={12} md={6} className="mb-3">
                    <FormGroup>
                      <Label for="email">Email</Label>
                      <Input
                        type="email"
                        name="email"
                        value={formData.email}
                        onChange={handleChange}
                        placeholder="Enter email address"
                      />
                    </FormGroup>
                  </Col>
                </Row>

                <Row>
                  <Col xs={12} md={6} className="mb-3">
                    <FormGroup>
                      <Label for="phone">Phone</Label>
                      <Input
                        type="text"
                        name="phone"
                        value={formData.phone}
                        onChange={handleChange}
                        placeholder="Enter phone number"
                      />
                    </FormGroup>
                  </Col>
                  <Col xs={12} md={6} className="mb-3">
                    <FormGroup>
                      <Label for="login_id">Employee ID (Login ID)</Label>
                      <Input
                        type="text"
                        name="login_id"
                        value={formData.login_id}
                        onChange={handleChange}
                        placeholder="Enter login ID"
                      />
                    </FormGroup>
                  </Col>
                </Row>

                <Row>
                  <Col xs={12} md={6} className="mb-3">
                    <FormGroup>
                      <Label for="password">Password</Label>
                      <Input
                        type="password"
                        name="password"
                        value={formData.password}
                        onChange={handleChange}
                        placeholder="Create a password"
                      />
                    </FormGroup>
                  </Col>
                  <Col xs={12} md={6} className="mb-3">
                    <FormGroup>
                      <Label for="about">About</Label>
                      <Input
                        type="text"
                        name="about"
                        value={formData.about}
                        onChange={handleChange}
                        placeholder="Short description"
                      />
                    </FormGroup>
                  </Col>
                </Row>

                <Row className="mb-2">
                  <Col xs={12} sm={6} className="mb-2">
                    <Button color="warning" className="w-100" type="button" onClick={backToDashboard}>
                      Back to Dashboard
                    </Button>
                  </Col>
                  <Col xs={12} sm={6}>
                    <Button color="primary" className="w-100" type="submit">
                      Register
                    </Button>
                  </Col>
                </Row>
              </Form>
            </CardBody>
          </Card>
        </Col>
      </Container>
    </div>
  );
};

export default Register;
