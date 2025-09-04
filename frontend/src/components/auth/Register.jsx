import { useEffect, useState } from "react";
import { useNavigate } from "react-router-dom";
import { useFormik } from "formik";
import * as Yup from "yup";
import { saveUser, updateUser } from "../../services/userService";
import { useAuth } from "../../context/AuthContext";
import { showAlert } from "../../utils/helpers";
import { fetchRoles } from "../../services/roleServices";

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
} from "reactstrap";

import "../../assets/styles.css";

const Register = ({ user, setEditingUser, refreshUsers }) => {


//    Fullstack Java Developer Vijay Chaurasiya

  const isEditMode = !!user;
  const handleCancel = () => setEditingUser(null);
  const navigate = useNavigate();
  const { token } = useAuth();
  const [roles, setRoles] = useState([]);

  useEffect(() => {
    console.log(user)
    if (token) {
      fetchRoles(setRoles, token);
    }
  }, [token]);

  const validationSchema = Yup.object({
    name: Yup.string().required("Name is required"),
    email: Yup.string().email("Invalid email").required("Email is required"),
    phone: Yup.string().required("Phone is required"),
    login_id: Yup.string().required("Login ID is required"),
    password: Yup.string()
      .min(4, "Password too short")
      .required("Password is required"),
    about: Yup.string().max(200, "Too long"),
    role: Yup.number().required("Role is required"),
  });

  const formik = useFormik({
    initialValues: {
      name: user?.name || "",
      email: user?.email || "",
      about: user?.about || "",
      password: "",
      phone: user?.phone || "",
      login_id: user?.username || "",
      role: user?.roles?.[0]?.role_id || "",
    },
    validationSchema,
    onSubmit: async (values) => {
      const userData = {
        name: values.name,
        email: values.email,
        about: values.about,
        password: values.password,
        phone: values.phone,
        username: values.login_id, // backend expects 'username'
        roles: [{ role_id: values.role }],
      };
      console.log(userData)

      try {
        if (isEditMode) {
          await updateUser(user.user_id, userData, token);
          showAlert("User Updated Successfully!", "success");
        } else {
          await saveUser(userData, token);
          showAlert("User Created Successfully!", "success");
        }
        refreshUsers();
        setEditingUser(null);
      } catch (error) {
        console.error("Error saving user:", error);
        showAlert(error.message || "Something went wrong!", "error");
      }
    },
  });

  return (
    <div className="wrapperStyle register-background" style={{ minHeight: "100vh" }}>
      <Container fluid className="py-4 px-2 d-flex justify-content-center align-items-center">
        <Col xs={12} sm={11} md={10} lg={9} xl={8} style={{ maxWidth: "960px" }}>
          <Card className="cardStyle shadow rounded-4 border-0">
            <CardBody className="p-5">
              <Form onSubmit={formik.handleSubmit}>
                <Row>
                  <Col md={6} className="mb-2">
                    <FormGroup>
                      <Label for="name">Name</Label>
                      <Input
                        type="text"
                        name="name"
                        value={formik.values.name}
                        onChange={formik.handleChange}
                        onBlur={formik.handleBlur}
                        invalid={formik.touched.name && !!formik.errors.name}
                        placeholder="Enter full name"
                      />
                      {formik.touched.name && formik.errors.name && (
                        <div className="text-danger">{formik.errors.name}</div>
                      )}
                    </FormGroup>
                  </Col>

                  <Col md={6} className="mb-2">
                    <FormGroup>
                      <Label for="email">Email</Label>
                      <Input
                        type="email"
                        name="email"
                        value={formik.values.email}
                        onChange={formik.handleChange}
                        onBlur={formik.handleBlur}
                        invalid={formik.touched.email && !!formik.errors.email}
                        placeholder="Enter email address"
                      />
                      {formik.touched.email && formik.errors.email && (
                        <div className="text-danger">{formik.errors.email}</div>
                      )}
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
                        value={formik.values.phone}
                        onChange={formik.handleChange}
                        onBlur={formik.handleBlur}
                        invalid={formik.touched.phone && !!formik.errors.phone}
                        placeholder="Enter phone number"
                      />
                      {formik.touched.phone && formik.errors.phone && (
                        <div className="text-danger">{formik.errors.phone}</div>
                      )}
                    </FormGroup>
                  </Col>

                  <Col md={6} className="mb-2">
                    <FormGroup>
                      <Label for="login_id">Employee ID (Login ID)</Label>
                      <Input
                        type="text"
                        name="login_id"
                        value={formik.values.login_id}
                        onChange={formik.handleChange}
                        onBlur={formik.handleBlur}
                        invalid={formik.touched.login_id && !!formik.errors.login_id}
                        placeholder="Enter login ID"
                      />
                      {formik.touched.login_id && formik.errors.login_id && (
                        <div className="text-danger">{formik.errors.login_id}</div>
                      )}
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
                        value={formik.values.password}
                        onChange={formik.handleChange}
                        onBlur={formik.handleBlur}
                        invalid={formik.touched.password && !!formik.errors.password}
                        placeholder="Create a password"
                      />
                      {formik.touched.password && formik.errors.password && (
                        <div className="text-danger">{formik.errors.password}</div>
                      )}
                    </FormGroup>
                  </Col>

                  <Col md={6} className="mb-2">
                    <FormGroup>
                      <Label for="about">About</Label>
                      <Input
                        type="text"
                        name="about"
                        value={formik.values.about}
                        onChange={formik.handleChange}
                        onBlur={formik.handleBlur}
                        invalid={formik.touched.about && !!formik.errors.about}
                        placeholder="Short description"
                      />
                      {formik.touched.about && formik.errors.about && (
                        <div className="text-danger">{formik.errors.about}</div>
                      )}
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
                        value={formik.values.role}
                        onChange={formik.handleChange}
                        onBlur={formik.handleBlur}
                        invalid={formik.touched.role && !!formik.errors.role}
                      >
                        <option value="">Select a role</option>
                        {roles.map((role) => (
                          <option key={role.role_id} value={role.role_id}>
                            {role.role_name}
                          </option>
                        ))}
                      </Input>
                      {formik.touched.role && formik.errors.role && (
                        <div className="text-danger">{formik.errors.role}</div>
                      )}
                    </FormGroup>
                  </Col>
                  <Col>
                    <Label>Status</Label>
                    {/* Future Status field or placeholder */}
                  </Col>
                </Row>

                <Row className="mb-3">
                  <Col xs="6">
                    <Button type="button" color="secondary" onClick={handleCancel} className="w-100">
                      Cancel
                    </Button>
                  </Col>
                  <Col xs="6">
                    <Button type="submit" color="success" className="w-100">
                      {isEditMode ? "Update User" : "Create User"}
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
