import React, { useState } from "react";
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
import { resetPassword } from "../../services/axios";

const ResetPassword = () => {
  const [loginId, setLoginId] = useState("");
  const [newPassword, setNewPassword] = useState("");
  const [message, setMessage] = useState("");
  const navigate = useNavigate();

  const handleResetPassword = async (e) => {
    e.preventDefault();
    const token = localStorage.getItem("token");

    if (!token) {
      setMessage("Reset token not found. Please verify OTP again.");
      return;
    }

    try {
      await resetPassword(loginId, newPassword, token);
      setMessage("Password has been reset successfully!");
      setTimeout(() => navigate("/home/login"), 1000);
    } catch (error) {
      console.error("Reset failed:", error);
      setMessage(error.message || "Failed to reset password. Please try again.");
    }
  };

  return (
    <div
      className="vh-100 d-flex align-items-center justify-content-center bg-image"
      style={{
        backgroundImage:
          "url('https://as2.ftcdn.net/jpg/06/12/69/39/1000_F_612693965_Ic0XfvkMa44xQXHA8lonULgqoEzyS0Xl.jpg')",
        backgroundSize: "cover",
      }}
    >
      <Container>
        <Row className="justify-content-center">
          <Col md={6} lg={5}>
            <Card className="shadow-lg rounded-4">
              <CardBody className="p-4 bg-white bg-opacity-75">
                <h2 className="text-center mb-3">
                  <i className="bi bi-key-fill me-2"></i>Reset Password
                </h2>
                <p className="text-center text-muted mb-4">
                  Enter your Login ID and new password to reset your account.
                </p>

                <Form onSubmit={handleResetPassword}>
                  <FormGroup>
                    <Label for="loginId">Login ID:</Label>
                    <Input
                      type="text"
                      id="loginId"
                      value={loginId}
                      onChange={(e) => setLoginId(e.target.value)}
                      placeholder="Enter Login ID"
                      required
                    />
                  </FormGroup>

                  <FormGroup>
                    <Label for="newPassword">New Password:</Label>
                    <Input
                      type="password"
                      id="newPassword"
                      value={newPassword}
                      onChange={(e) => setNewPassword(e.target.value)}
                      placeholder="Enter New Password"
                      required
                    />
                  </FormGroup>

                  <div className="d-grid">
                    <Button color="danger" type="submit">
                      <i className="bi bi-shield-lock-fill me-1"></i> Reset Password
                    </Button>
                  </div>
                </Form>

                {message && (
                  <Alert color="info" className="mt-3 text-center">
                    {message}
                  </Alert>
                )}

                <div className="text-center mt-3">
                  <Link to="/home/login" className="text-decoration-none">
                    <i className="bi bi-arrow-left me-1"></i> Back to Login
                  </Link>
                </div>
              </CardBody>
            </Card>
          </Col>
        </Row>
      </Container>
    </div>
  );
};

export default ResetPassword;
