import React, { useState, useEffect } from "react";
import { useNavigate } from "react-router-dom";
import { saveUser } from "../../services/axios";
import { useAuth } from "../../context/AuthContext";

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
import {fetchRoles} from "../../services/roleServices"

const Register = () => {

  
  const navigate = useNavigate();
  const { token } = useAuth();
  const [formData, setFormData] = useState({
    name: "",
    email: "",
    about: "",
    password: "",
    phone: "",
    login_id: "",
    role: "", // Add role to the form data
  });

  const [error, setError] = useState("");
  const [roles, setRoles] = useState([]); // State to hold roles
  const [roleError, setRoleError] = useState("");

  // Fetch roles from API
  // const fetchRoles = async () => {
  //   try {
  //     const response = await fetch("http://localhost:8081/dms/role/", {
  //       method: "GET",
  //       headers: {
  //         "Authorization": `Bearer ${token}`,
  //         "Content-Type": "application/json",
  //       },
  //     });

  //     if (!response.ok) {
  //       throw new Error("Error fetching roles.");
  //     }

  //     const data = await response.json();
  //     if (data.status) {
  //       setRoles(data.data); // Set the roles from the 'data' field
  //     }
  //   } catch (err) {
  //     setError("Error fetching roles.");
  //   }
  // };

  useEffect(() => {
    fetchRoles(token,setRoles);
  }, []);

  const handleChange = (e) => {
    setFormData((prev) => ({
      ...prev,
      [e.target.name]: e.target.value,
    }));
  };

  const handleFormSubmit = async (e) => {
    e.preventDefault();

    // Structure the user data with role correctly
    const { name, email, about, password, phone, login_id, role } = formData;
    const userData = {
      name,
      email,
      about,
      password,
      phone,
      login_id,
      user_roles: [{ role_id: role }], // Wrap the role in an array
    };

    try {
      await saveUser(userData, navigate, token); // Ensure this method is implemented correctly in axios
    } catch (error) {
      console.error("Error saving user:", error);
    }
  };

  const backToDashboard = () => {
    navigate("/dms/home/admindashboard");
  };

  return (
    <div className="register-background wrapperStyle" style={{ minHeight: "100vh" }}>
      <Container fluid className="d-flex justify-content-center align-items-center py-4 px-2">
        <Col xs={12} sm={11} md={8} lg={6} xl={5}>
          <Card className="cardStyle shadow" style={{ maxHeight: "90vh", overflowY: "auto" }}>
            <CardBody className="p-4">
              <h3 className="text-center text-primary mb-4 shimmer-text">Create User</h3>
              {error && <Alert color="danger">{error}</Alert>}
              <Form onSubmit={handleFormSubmit}> {/* Updated to handleFormSubmit */}
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

                {/* Role Selection */}
                <Row>
                  <Col xs={12} md={6} className="mb-3">
                    <FormGroup>
                      <Label for="role">Role</Label>
                      <Input
                        type="select"
                        name="role"
                        value={formData.role}
                        onChange={handleChange}
                      >
                        <option value="">Select a role</option>
                        {roles.map((role) => (
                          <option key={role.role_id} value={role.role_id}>
                            {role.role_name}
                          </option>
                        ))}
                      </Input>
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
