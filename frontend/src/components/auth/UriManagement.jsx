import {
  Card,
  CardBody,
  Form,
  FormGroup,
  Label,
  Input,
  Button,
  Row,
  Col,
} from "reactstrap";
import { useState } from "react";
import { useAuth } from "../../context/AuthContext";
import { showAlert } from "../../utils/helpers";
import axios from "../../" // assuming your axios instance handles base URL and headers

const UriManagement = () => {
  const [requestUri, setRequestUri] = useState("");
  const [requestMethod, setRequestMethod] = useState("GET");
  const { token } = useAuth();

  const handleCreateUri = async (e) => {
    e.preventDefault();

    if (!requestUri.trim()) {
      showAlert("URI cannot be empty.", "warning");
      return;
    }

    try {
      const response = await axios.post(
        "/object/create",
        {
          request_uri: requestUri,
          request_method: requestMethod,
        },
        {
          headers: {
            Authorization: `Bearer ${token}`,
          },
        }
      );

      if (response.data) {
        showAlert("URI created successfully!", "success");
        setRequestUri("");
        setRequestMethod("GET");
      }
    } catch (error) {
      console.error("URI creation failed:", error);
      const message = error?.response?.data?.message || "Failed to create URI.";
      showAlert(message, "error");
    }
  };

  return (
    <Row className="justify-content-center mt-4">
      <Col md={6}>
        <Card className="shadow-sm">
          <CardBody>
            <h5 className="text-primary mb-3">Create New URI</h5>
            <Form onSubmit={handleCreateUri}>
              <FormGroup>
                <Label for="requestUri">Request URI</Label>
                <Input
                  id="requestUri"
                  type="text"
                  placeholder="Enter URI e.g., /api/example"
                  value={requestUri}
                  onChange={(e) => setRequestUri(e.target.value)}
                />
              </FormGroup>

              <FormGroup>
                <Label for="requestMethod">Request Method</Label>
                <Input
                  id="requestMethod"
                  type="select"
                  value={requestMethod}
                  onChange={(e) => setRequestMethod(e.target.value)}
                >
                  <option value="GET">GET</option>
                  <option value="POST">POST</option>
                  <option value="PUT">PUT</option>
                  <option value="DELETE">DELETE</option>
                </Input>
              </FormGroup>

              <Button color="primary" type="submit" className="w-100">
                Create URI
              </Button>
            </Form>
          </CardBody>
        </Card>
      </Col>
    </Row>
  );
};

export default UriManagement;