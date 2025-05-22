import React, { useEffect, useState } from "react";
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
import { FaLink } from "react-icons/fa";
import Select from "react-select";
import { useAuth } from "../../context/AuthContext";
import { showAlert } from "../../utils/helpers";
import { fetchRoles } from "../../services/roleServices";
import {
  registerUriWithRoles,
  assignRolesToUri,
  deassignRolesFromUri,
} from "../../services/userService";

const UriManagement = () => {
  const { token } = useAuth();
  const [roles, setRoles] = useState([]);

  const [formState, setFormState] = useState({
    create: { uri: "", method: "GET", selectedRoles: [] },
    assign: { uri: "", method: "GET", selectedRoles: [] },
    deassign: { uri: "", method: "GET", selectedRoles: [] },
  });

  useEffect(() => {
    fetchRoles(setRoles, token);
  }, [token]);

  const handleInputChange = (section, field, value) => {
    setFormState((prev) => ({
      ...prev,
      [section]: {
        ...prev[section],
        [field]: value,
      },
    }));
  };

  const handleRoleChange = (section) => (selectedOptions) => {
    handleInputChange(section, "selectedRoles", selectedOptions || []);
  };

  const buildPayload = (section) => {
    return {
      object: {
        request_uri: formState[section].uri,
        request_method: formState[section].method,
      },
      roles: formState[section].selectedRoles.map((role) => ({ role_id: role.value })),
    };
  };

  const handleSubmit = async (e, section, serviceMethod, successMsg) => {
    e.preventDefault();
    try {
      const payload = buildPayload(section);
      await serviceMethod(payload, token);
      showAlert(successMsg, "success");
      setFormState((prev) => ({
        ...prev,
        [section]: { uri: "", method: "GET", selectedRoles: [] },
      }));
    } catch (error) {
      console.error(error);
      showAlert(error.message || `Failed to ${section} URI`, "danger");
    }
  };

  const roleOptions = roles.map((role) => ({ value: role.role_id, label: role.role_name }));

  const renderForm = (section, onSubmitHandler, buttonText, buttonColor) => (
    <Card className="shadow-sm bg-light-subtle h-100 mb-3">
      <CardBody>
        <h6 className="text-primary mb-3">
          <FaLink className="me-2" />
          {buttonText} URI
        </h6>
        <Form onSubmit={onSubmitHandler}>
          <FormGroup>
            <Label>URI</Label>
            <Input
              value={formState[section].uri}
              onChange={(e) => handleInputChange(section, "uri", e.target.value)}
              placeholder="/dms/example"
            />
          </FormGroup>
          <FormGroup>
            <Label>Method</Label>
            <Input
              type="select"
              value={formState[section].method}
              onChange={(e) => handleInputChange(section, "method", e.target.value)}
            >
              <option>GET</option>
              <option>POST</option>
              <option>PUT</option>
              <option>DELETE</option>
            </Input>
          </FormGroup>
          <FormGroup>
            <Label>Roles</Label>
            <Select
              isMulti
              options={roleOptions}
              value={formState[section].selectedRoles}
              onChange={handleRoleChange(section)}
              classNamePrefix="react-select"
              placeholder="Select Roles..."
            />
          </FormGroup>
          <Button color={buttonColor} type="submit" className="w-100">
            {buttonText} URI
          </Button>
        </Form>
      </CardBody>
    </Card>
  );

  return (
    <div>
      <Row className="gy-3">
        <Col md={4}>{renderForm("create", (e) => handleSubmit(e, "create", registerUriWithRoles, "URI created and roles assigned successfully!"), "Create", "success")}</Col>
        <Col md={4}>{renderForm("assign", (e) => handleSubmit(e, "assign", assignRolesToUri, "Roles assigned successfully!"), "Assign", "primary")}</Col>
        <Col md={4}>{renderForm("deassign", (e) => handleSubmit(e, "deassign", deassignRolesFromUri, "Roles de-assigned successfully!"), "De-Assign", "danger")}</Col>
      </Row>
    </div>
  );
};

export default UriManagement;
