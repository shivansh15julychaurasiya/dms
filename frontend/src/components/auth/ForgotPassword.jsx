import React, { useState, useEffect } from "react";
import { Link, useNavigate } from "react-router-dom";
import 'bootstrap/dist/css/bootstrap.min.css';

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
import { requestOtp, verifyOtp } from "../../services/userService";
import "../../assets/styles.css"
import { showAlert } from "../../utils/helpers";


const ForgotPassword = () => {
  const [loginId, setLoginId] = useState("");
  const [otp, setOtp] = useState("");
  const [message, setMessage] = useState("");
  const [otpSent, setOtpSent] = useState(false);
  const [timer, setTimer] = useState(120);
  const navigate = useNavigate();

  useEffect(() => {
    let interval;
    if (otpSent && timer > 0) {
      interval = setInterval(() => {
        setTimer((prev) => prev - 1);
      }, 1000);
    } else if (timer === 0) {
      clearInterval(interval);
    }
    return () => clearInterval(interval);
  }, [otpSent, timer]);

  const formatTime = (seconds) => {
    const m = Math.floor(seconds / 60).toString().padStart(2, "0");
    const s = (seconds % 60).toString().padStart(2, "0");
    return `${m}:${s}`;
  };

  const handleSendOtp = (e) => {
    e.preventDefault();
    requestOtp(loginId, setMessage, setOtpSent, setTimer);
  };

  const handleVerifyOtp = (e) => {
    e.preventDefault();
    verifyOtp(loginId, otp, setMessage, navigate);
    showAlert("OTP verified Successfully 1")
  };

  return (
    <div className="vh-100 d-flex align-items-center justify-content-center register-background">
      <Container>
        <Row className="justify-content-center">
          <Col md={6} lg={5}>
            <Card className="shadow-lg border-1 rounded-4 cardStyle">
              <CardBody className="p-4 ">
                <h2 className="text-center mb-3 text-primary">
                  <i className="bi bi-unlock me-2"></i>Forgot Password
                </h2>
                <p className="text-center text-muted mb-4">
                  {!otpSent
                    ? "Enter your Login ID to receive an OTP for password reset."
                    : "Enter the OTP sent to your registered contact."}
                </p>

                {otpSent && timer > 0 && (
                  <p className="text-center text-muted">
                    Time remaining: <strong>{formatTime(timer)}</strong>
                  </p>
                )}

                <Form onSubmit={otpSent ? handleVerifyOtp : handleSendOtp}>
                  <FormGroup>
                    <Label for="loginId" className="fw-bold text-primary">{otpSent ? "Enter OTP:" : "Login ID:"}</Label>
                    <Input
                      type="text"
                      id={otpSent ? "otp" : "loginId"}
                      value={otpSent ? otp : loginId}
                      onChange={(e) => otpSent ? setOtp(e.target.value) : setLoginId(e.target.value)}
                      placeholder={otpSent ? "Enter OTP" : "Enter Login ID"}
                      required
                      disabled={otpSent && timer === 0}
                      className="rounded-3 border-2"
                    />
                  </FormGroup>

                  <div className="d-grid">
                    {!otpSent ? (
                      <Button color="danger" className="btn-block rounded-pill py-2 fs-5 hover-shadow" type="submit">
                        <i className="bi bi-envelope-arrow-up me-1"></i> Send Reset OTP
                      </Button>
                    ) : timer > 0 ? (
                      <Button color="danger" className="btn-block rounded-pill py-2 fs-5 hover-shadow" type="submit">
                        <i className="bi bi-shield-lock me-1"></i> Verify OTP
                      </Button>
                    ) : (
                      <Button color="warning" className="btn-block rounded-pill py-2 fs-5 hover-shadow" type="button" onClick={handleSendOtp}>
                        <i className="bi bi-arrow-clockwise me-1"></i> Resend OTP
                      </Button>
                    )}
                  </div>
                </Form>

                {message && (
                  <Alert color="info" className="mt-3 text-center">
                    {message}
                  </Alert>
                )}

                <div className="text-center mt-3">
                  <Link to="/" className="text-decoration-none text-danger fw-bold">
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

export default ForgotPassword;
