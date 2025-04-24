import React from "react";
import { Link } from "react-router-dom";
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

const EditProfile = () => {
  return (
    <Container fluid className="min-vh-100 d-flex align-items-center justify-content-center bg-light register-background">
      <Row className="w-100 justify-content-center">
        <Col md={6} lg={5}>
          <Card className="shadow-lg border-0">
            <CardBody>
              <h2 className="text-center mb-4 text-primary fw-bold">
                <i className="bi bi-pencil-square me-2"></i>Edit Profile
              </h2>

              <Form>
                <FormGroup>
                  <Label for="name" className="fw-bold text-primary">
                    Full Name
                  </Label>
                  <Input
                    type="text"
                    id="name"
                    placeholder="Enter full name"
                  />
                </FormGroup>

                <FormGroup>
                  <Label for="email" className="fw-bold text-primary">
                    Email Address
                  </Label>
                  <Input
                    type="email"
                    id="email"
                    placeholder="Enter email"
                  />
                </FormGroup>

                <FormGroup>
                  <Label for="oldPassword" className="fw-bold text-primary">
                    Old Password
                  </Label>
                  <Input
                    type="password"
                    id="oldPassword"
                    placeholder="Enter old password"
                  />
                </FormGroup>

                <FormGroup>
                  <Label for="newPassword" className="fw-bold text-primary">
                    New Password
                  </Label>
                  <Input
                    type="password"
                    id="newPassword"
                    placeholder="Enter new password"
                  />
                </FormGroup>

                <div className="d-grid">
                  <Button color="primary" type="submit">
                    <i className="bi bi-save me-1"></i> Save Changes
                  </Button>
                </div>

                <div className="text-center mt-3">
                  <Link to="/" className="text-decoration-none text-danger fw-bold">
                    <i className="bi bi-arrow-left-circle me-1"></i>Back to Home
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

export default EditProfile;
