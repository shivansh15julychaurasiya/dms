import React, { useState, useEffect } from "react";
import { useNavigate } from "react-router-dom";
import { saveUser } from "../../services/userService";
import { useAuth } from "../../context/AuthContext";
import { showAlert } from "../../utils/helpers";
import {
  Container,
  Row,
  Col,
  Card,
  CardHeader,
  CardBody,
  Form,
  FormGroup,
  Label,
  Input,
  Button,
  Alert,
} from "reactstrap";
import "../../assets/styles.css";
import { fetchRoles } from "../../services/roleServices";

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
    role: "",
  });

  const [error, setError] = useState("");
  const [roles, setRoles] = useState([]);
  const [roleError, setRoleError] = useState("");

  useEffect(() => {
    fetchRoles(token, setRoles);
  }, [token]);

  const handleChange = (e) => {
    setFormData((prev) => ({
      ...prev,
      [e.target.name]: e.target.value,
    }));
  };

  const handleFormSubmit = async (e) => {
    e.preventDefault();
    const { name, email, about, password, phone, login_id, role } = formData;
    const userData = {
      name,
      email,
      about,
      password,
      phone,
      login_id,
      user_roles: [{ role_id: role }],
    };

    try {
      await saveUser(userData, navigate, token);
      showAlert("User Create Successfully!","success")
    } catch (error) {
      console.error("Error saving user:", error);
      showAlert("Something went wrong!","error")

    }
  };

  const backToDashboard = () => {
    navigate("/home/admindashboard");
  };

  return (
    <div className="register-background wrapperStyle" style={{ minHeight: "100vh" }}>
      <Container fluid className="py-4 px-2 d-flex justify-content-center align-items-center">
        <Col xs={12} sm={11} md={9} lg={8} xl={6}>
          <Card className="cardStyle shadow">
          <CardHeader className="bg-primary text-white text-center fs-4 rounded-3"> Create User</CardHeader>
            <CardBody className="p-4d ">
              {error && <Alert color="danger">{error}</Alert>}
              <Form onSubmit={handleFormSubmit}>
                <Row>
                  <Col md={6} className="mb-2">
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
                  <Col md={6} className="mb-2">
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
                  <Col md={6} className="mb-2">
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
                  <Col md={6} className="mb-2">
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
                  <Col md={6} className="mb-2">
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
                  <Col md={6} className="mb-2">
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

                <Row>
                  <Col md={6} className="mb-2">
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
                  <Col sm={6} className="mb-2">
                    <Button color="warning" className="w-100" type="button" onClick={backToDashboard}>
                      Back to Dashboard
                    </Button>
                  </Col>
                  <Col sm={6}>
                    <Button color="success" className="w-100" type="submit">
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
