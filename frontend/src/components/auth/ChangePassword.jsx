import React from "react";
import { Container, Row, Col, Card, CardBody, Form, FormGroup, Label, Input, Button } from "reactstrap";
import { useFormik } from "formik";
import * as Yup from "yup";
import { changePassword } from "../../services/userService";
import { showAlert } from "../../utils/helpers";

const ChangePassword = () => {
  const validationSchema = Yup.object({
    username: Yup.string().required("Username is required"),
    oldPassword: Yup.string().required("Old Password is required"),
    newPassword: Yup.string()
      .required("New Password is required")
      .min(4, "Password must be at least 4 characters"),
  });

  const formik = useFormik({
    initialValues: {
      username: "",
      oldPassword: "",
      newPassword: "",
    },
    validationSchema: validationSchema,
    onSubmit: async (values, { resetForm }) => {
      try {
        await changePassword(values.username, values.oldPassword, values.newPassword);
        showAlert("Password changed successfully!", "success");
        resetForm();
      } catch (error) {
        console.error("Password change failed:", error);
        showAlert("Failed to change password. Please try again.", "error");
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
                <i className="bi bi-shield-lock me-2"></i>Change Password
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
                    onBlur={formik.handleBlur}
                    className="rounded-3 border-2"
                    placeholder="Enter your username"
                    invalid={formik.touched.username && !!formik.errors.username}
                  />
                </FormGroup>

                <FormGroup>
                  <Label for="oldPassword" className="fw-bold text-primary">Old Password:</Label>
                  <Input
                    type="password"
                    id="oldPassword"
                    name="oldPassword"
                    value={formik.values.oldPassword}
                    onChange={formik.handleChange}
                    onBlur={formik.handleBlur}
                    className="rounded-3 border-2"
                    placeholder="Enter old password"
                    invalid={formik.touched.oldPassword && !!formik.errors.oldPassword}
                  />
                </FormGroup>

                <FormGroup>
                  <Label for="newPassword" className="fw-bold text-primary">New Password:</Label>
                  <Input
                    type="password"
                    id="newPassword"
                    name="newPassword"
                    value={formik.values.newPassword}
                    onChange={formik.handleChange}
                    onBlur={formik.handleBlur}
                    className="rounded-3 border-2"
                    placeholder="Enter new password"
                    invalid={formik.touched.newPassword && !!formik.errors.newPassword}
                  />
                </FormGroup>

                <div className="d-grid">
                  <Button color="primary" type="submit" className="py-2 fs-5 rounded-pill hover-shadow">
                    <i className="bi bi-key-fill me-1"></i> Change Password
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
