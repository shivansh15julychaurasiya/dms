import React, { useContext } from "react";
import { Link, useNavigate } from "react-router-dom";
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
import { useFormik } from "formik";
import * as Yup from "yup";
import { loginUser } from "../../services/userService";
import { getRoleRedirectPath, showAlert } from "../../utils/helpers";
import { AuthContext } from "../../context/AuthContext";
import gov1 from "../../assets/gov1.png";

const Login = () => {
  const { login } = useContext(AuthContext);
  const navigate = useNavigate();

  // Formik validation schema
  const validationSchema = Yup.object({
    loginId: Yup.number()
      .required("User ID is required")
      .positive("User ID must be a positive number"),
    password: Yup.string()
      .required("Password is required")
      .min(4, "Password must be at least 6 characters long"),
  });

  // Formik form handling
  const formik = useFormik({
    initialValues: {
      loginId: "",
      password: "",
    },
    validationSchema: validationSchema,
    onSubmit: async (values) => {
      try {
        const { token, user } = await loginUser(
          values.loginId,
          values.password
        );
        login({ token, user });

        const roleNames = user.roles.map((role) => role.role_name);
        showAlert("Login successful", "success");
        navigate(getRoleRedirectPath(roleNames));
      } catch {
        showAlert("Invalid User ID or Password", "error");
      }
    },
  });

  return (
    <Container
      fluid
      className="min-vh-100 d-flex justify-content-center align-items-center register-background"
    >
      <Row className="w-100 justify-content-center">
        <Col md={6} lg={4}>
          <Card className="shadow-lg border-1 rounded-4 cardStyle ">
            <img
              className="rounded mt-2"
              src={gov1}
              alt="Government Logo"
              style={{
                width: "75px",
                height: "75px",
                display: "block",
                marginLeft: "auto",
                marginRight: "auto",
                marginBottom: "2px", // Set 3px space below image
              }}
            />
            <h2
              className="text-center text-primary fw-bold mb-0"
              style={{ marginTop: "0px" }}
            >
              High Court
            </h2>
            <CardBody>
              <Form onSubmit={formik.handleSubmit}>
                <FormGroup>
                  <Label for="loginId" className="fw-bold text-primary">
                    User ID:
                  </Label>
                  <Input
                    type="number"
                    id="loginId"
                    name="loginId"
                    value={formik.values.loginId}
                    onChange={formik.handleChange}
                    onBlur={formik.handleBlur}
                    className="rounded-3 border-2"
                    placeholder="Enter your User ID"
                    invalid={formik.touched.loginId && !!formik.errors.loginId}
                  />
                </FormGroup>
                <FormGroup>
                  <Label for="password" className="fw-bold text-primary">
                    Password:
                  </Label>
                  <Input
                    type="password"
                    id="password"
                    name="password"
                    value={formik.values.password}
                    onChange={formik.handleChange}
                    onBlur={formik.handleBlur}
                    className="rounded-3 border-2"
                    placeholder="Enter your password"
                    invalid={
                      formik.touched.password && !!formik.errors.password
                    }
                  />
                </FormGroup>
                <div className="d-grid">
                  <Button
                    color="primary"
                    type="submit"
                    className="py-2 fs-5 rounded-pill hover-shadow"
                  >
                    <i className="bi bi-box-arrow-in-right me-1"></i> Login
                  </Button>
                </div>
                <div className="text-center mt-3">
                  <Link
                    to="/home/forgot"
                    className="text-danger fw-bold text-decoration-none"
                  >
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
