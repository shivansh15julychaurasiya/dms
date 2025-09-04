import React from "react";
import { useNavigate } from "react-router-dom";
import {
  Container, Row, Col, Card, CardBody,
  Form, FormGroup, Label, Input, Button
} from "reactstrap";
import { useFormik } from "formik";
import { changePassword } from "../../services/userService";
import { showAlert } from "../../utils/helpers";
import { useAuth } from "../../context/AuthContext";

const ChangePassword = () => {


  //    Fullstack Java Developer Vijay Chaurasiya
  
  const navigate = useNavigate();
  const { token } = useAuth();

  const formik = useFormik({
    initialValues: {
      username: "",
      old_password: "",
      new_password: "",
    },
    onSubmit: async (values) => {
      try {
        await changePassword(values.username,values.old_password,values.new_password, token);
        showAlert("Password reset successfully", "success");
        navigate("/home/admindashboard");
      } catch (error) {
        showAlert("Password reset failed. Please check your credentials.", "error");
      }
    },
  });

  return (
    <Container fluid className="min-vh-100 d-flex justify-content-center align-items-center register-background">
      <Row className="w-100 justify-content-center">
        <Col md={6} lg={4}>
          <Card className="shadow-lg border-1 rounded-4 cardStyle">
            <CardBody>
              <h2 className="text-center text-primary fw-bold mb-4">
                <i className="bi bi-shield-lock-fill me-2"></i>
                Reset Password
              </h2>

              <Form onSubmit={formik.handleSubmit}>
                <FormGroup>
                  <Label for="username" className="fw-bold text-primary">Username:</Label>
                  <Input
                    type="text"
                    id="username"
                    name="username"
                    value={formik.values.username}
                    onChange={formik.handleChange}
                    className="rounded-3 border-2"
                    placeholder="Enter your username"
                    required
                  />
                </FormGroup>

                <FormGroup>
                  <Label for="old_password" className="fw-bold text-primary">Old Password:</Label>
                  <Input
                    type="password"
                    id="old_password"
                    name="old_password"
                    value={formik.values.old_password}
                    onChange={formik.handleChange}
                    className="rounded-3 border-2"
                    placeholder="Enter old password"
                    required
                  />
                </FormGroup>

                <FormGroup>
                  <Label for="new_password" className="fw-bold text-primary">New Password:</Label>
                  <Input
                    type="password"
                    id="new_password"
                    name="new_password"
                    value={formik.values.new_password}
                    onChange={formik.handleChange}
                    className="rounded-3 border-2"
                    placeholder="Enter new password"
                    required
                  />
                </FormGroup>

                <div className="d-grid mt-3">
                  <Button type="submit" color="primary" className="py-2 fs-5 rounded-pill">
                    Reset Password
                  </Button>
                </div>
              </Form>
            </CardBody>
          </Card>
        </Col>
      </Row>
    </Container>
  );
};

export default ChangePassword;
