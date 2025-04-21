import React, { useState, useEffect } from "react";
import axios from "axios";
import "bootstrap/dist/css/bootstrap.min.css";
import "bootstrap-icons/font/bootstrap-icons.css";
import { Link, useNavigate } from "react-router-dom";

const ForgotPassword = () => {
  const [loginId, setLoginId] = useState("");
  const [otp, setOtp] = useState("");
  const [message, setMessage] = useState("");
  const [otpSent, setOtpSent] = useState(false);
  const [timer, setTimer] = useState(120); // 2 minutes
  const navigate = useNavigate();

  // Start countdown timer after OTP is sent
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
    const m = Math.floor(seconds / 60)
      .toString()
      .padStart(2, "0");
    const s = (seconds % 60).toString().padStart(2, "0");
    return `${m}:${s}`;
  };

  const handleSendOtp = async (e) => {
    e.preventDefault();
    try {
      const response = await axios.post(
        "http://localhost:8081/dms/auth/request-otp",
        {
          login_id: loginId,
          otp_type: "Reset",
        }
      );
      console.log(response);
      setMessage("OTP has been sent successfully!");

      setTimeout(() => setMessage(""), 3000);
      setOtpSent(true);
      setTimer(120); // Reset timer
    } catch (error) {
      console.error(error);
      setMessage("Failed to send OTP. Please check the Login ID.");
    }
  };

  const handleVerifyOtp = async (e) => {
    e.preventDefault();
    try {
      const response = await axios.post(
        "http://localhost:8081/dms/auth/verify-reset-otp",
        {
          username: loginId,
          otp: otp,
        }
      );
      console.log(response.data.data.token);
      localStorage.setItem("token", response.data.data.token);
      navigate("/home/reset");
      setMessage("OTP has been verified successfully!");
    } catch (error) {
      console.error(error);
      setMessage("Failed to verify OTP. Please check the Login ID.");
    }
  };

  return (
    <div
      style={{
        height: "100vh",
        backgroundImage:
          "url('https://as2.ftcdn.net/jpg/06/12/69/39/1000_F_612693965_Ic0XfvkMa44xQXHA8lonULgqoEzyS0Xl.jpg')",
        backgroundSize: "cover",
        backgroundPosition: "center",
        display: "flex",
        alignItems: "center",
        justifyContent: "center",
      }}
    >
      <div
        style={{
          backgroundColor: "rgba(255, 255, 255, 0.95)",
          padding: "2rem",
          borderRadius: "1.5rem",
          maxWidth: "400px",
          width: "100%",
          boxShadow: "0 4px 20px rgba(0, 0, 0, 0.3)",
        }}
      >
        <h2 className="text-center mb-4">
          <i className="bi bi-unlock me-2"></i>Forgot Password
        </h2>
        <p className="text-muted text-center mb-4">
          {!otpSent
            ? "Enter your Login ID to receive an OTP for password reset."
            : "Enter the OTP sent to your registered contact."}
        </p>

        {otpSent && timer > 0 && (
          <p className="text-center text-muted mb-3">
            Time remaining: <strong>{formatTime(timer)}</strong>
          </p>
        )}

        <form onSubmit={otpSent ? handleVerifyOtp : handleSendOtp}>
          {!otpSent ? (
            <div className="mb-3">
              <label htmlFor="loginId" className="form-label">
                Login ID:
              </label>
              <input
                type="text"
                className="form-control"
                id="loginId"
                value={loginId}
                onChange={(e) => setLoginId(e.target.value)}
                placeholder="Enter Login ID"
                required
              />
            </div>
          ) : (
            <div className="mb-3">
              <label htmlFor="otp" className="form-label">
                Enter OTP:
              </label>
              <input
                type="text"
                className="form-control"
                id="otp"
                value={otp}
                onChange={(e) => setOtp(e.target.value)}
                placeholder="Enter OTP"
                required
                disabled={timer === 0}
              />
            </div>
          )}

          <div className="d-grid">
            {!otpSent ? (
              <button
                type="submit"
                className="btn btn-danger"
                onMouseOver={(e) =>
                  (e.target.style.backgroundColor = "#e64949")
                }
                onMouseOut={(e) => (e.target.style.backgroundColor = "#ff6b6b")}
                style={{ backgroundColor: "#ff6b6b", border: "none" }}
              >
                <i className="bi bi-envelope-arrow-up me-1"></i> Send Reset OTP
              </button>
            ) : timer > 0 ? (
              <button
                type="submit"
                className="btn btn-danger"
                onMouseOver={(e) =>
                  (e.target.style.backgroundColor = "#e64949")
                }
                onMouseOut={(e) => (e.target.style.backgroundColor = "#ff6b6b")}
                style={{ backgroundColor: "#ff6b6b", border: "none" }}
              >
                <i className="bi bi-shield-lock me-1"></i> Verify OTP
              </button>
            ) : (
              <button
                type="button"
                className="btn btn-warning"
                onClick={handleSendOtp}
              >
                <i className="bi bi-arrow-clockwise me-1"></i> Resend OTP
              </button>
            )}
          </div>
        </form>

        {message && (
          <div className="alert alert-info mt-3 text-center" role="alert">
            {message}
          </div>
        )}

        <div className="text-center mt-3">
          <Link to="/home/login" className="text-decoration-none">
            <i className="bi bi-arrow-left me-1"></i> Back to Login
          </Link>
        </div>
      </div>
    </div>
  );
};

export default ForgotPassword;
