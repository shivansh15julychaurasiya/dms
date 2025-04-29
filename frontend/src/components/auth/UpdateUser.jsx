import React, { useState, useContext, useEffect } from "react";
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
import { useNavigate, useParams } from "react-router-dom";
import { AuthContext } from "../../context/AuthContext";
import { showAlert } from "../../utils/helpers";
import { getUserById, updateUser } from "../../services/userService"; // ✅ imported helper functions

const UpdateUser = () => {
  const { token } = useContext(AuthContext);
  const { userId } = useParams();
  const navigate = useNavigate();

  const [formData, setFormData] = useState({
    name: "",
    email: "",
    about: "",
    password: "",
  });

  const [errorMsg, setErrorMsg] = useState("");
  const [successMsg, setSuccessMsg] = useState("");

  useEffect(() => {
    const fetchUser = async () => {
      try {
        const data = await getUserById(userId, token);
        setFormData({
          name: data.data.name || "", // Ensure default empty string if undefined
          email: data.data.email || "", // Ensure default empty string if undefined
          about: data.data.about || "", // Ensure default empty string if undefined
          password: "", // You might want to leave password as empty string
        });
        // console.log(data.data.name)
      } catch (error) {
        setErrorMsg("Failed to load user data.");
      }
    };
    fetchUser();
  }, [userId, token]);

  const handleChange = (e) => {
    setFormData((prev) => ({
      ...prev,
      [e.target.name]: e.target.value,
    }));
  };

  const handleUpdate = async (e) => {
    e.preventDefault();
    setErrorMsg("");
    setSuccessMsg("");

    try {
      await updateUser(userId, formData, token);
      setSuccessMsg("User updated successfully!");
      setTimeout(() => navigate("/home/admindashboard"), 2000);
    } catch (err) {
      showAlert("Failed to update user!");
      setErrorMsg("Update failed. Please try again.");
    }
  };

  return (
    <Container fluid className="min-vh-100 d-flex justify-content-center align-items-center register-background">
      <Row className="w-100 justify-content-center">
        <Col md={6} lg={5}>
          <Card className="shadow-lg rounded-4 border-1 cardStyle">
            <CardBody>
              <h2 className="text-center text-primary fw-bold mb-4">
                <i className="bi bi-pencil-square me-2"></i>Update User
              </h2>
              {errorMsg && <Alert color="danger">{errorMsg}</Alert>}
              {successMsg && <Alert color="success">{successMsg}</Alert>}
              <Form onSubmit={handleUpdate}>
                <FormGroup>
                  <Label for="name" className="fw-bold text-primary">Name:</Label>
                  <Input
                    type="text"
                    id="name"
                    name="name"
                    value={formData.name || ""}
                    onChange={handleChange}
                    className="rounded-3 border-2"
                    placeholder="Enter name"
                    required
                  />
                </FormGroup>

                <FormGroup>
                  <Label for="email" className="fw-bold text-primary">Email:</Label>
                  <Input
                    type="email"
                    id="email"
                    name="email"
                    value={formData.email || ""}
                    onChange={handleChange}
                    className="rounded-3 border-2"
                    placeholder="Enter email"
                    required
                  />
                </FormGroup>

                <FormGroup>
                  <Label for="about" className="fw-bold text-primary">About:</Label>
                  <Input
                    type="text"
                    id="about"
                    name="about"
                    value={formData.about || ""}
                    onChange={handleChange}
                    className="rounded-3 border-2"
                    placeholder="Write something..."
                    required
                  />
                </FormGroup>

                <FormGroup>
                  <Label for="password" className="fw-bold text-primary">Password:</Label>
                  <Input
                    type="password"
                    id="password"
                    name="password"
                    value={formData.password || ""}
                    onChange={handleChange}
                    className="rounded-3 border-2"
                    placeholder="New password"
                    required
                  />
                </FormGroup>

                <div className="d-grid mt-3">
                  <Button color="primary" type="submit" className="py-2 fs-5 rounded-pill hover-shadow">
                    <i className="bi bi-check2-circle me-1"></i> Update User
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

export default UpdateUser;
