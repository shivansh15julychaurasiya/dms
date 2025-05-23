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
  Pagination,
  PaginationItem,
  PaginationLink,
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
  fetchPaginatedObjects,
} from "../../services/userService"; // Ensure this is paginated version

const UriManagement = () => {
  const { token } = useAuth();

  const [roles, setRoles] = useState([]);
  const [objects, setObjects] = useState([]);

  // Pagination state
  const [currentPage, setCurrentPage] = useState(0); // 0-based index
  const [pageSize] = useState(5);
  const [totalPages, setTotalPages] = useState(0);

  const [formState, setFormState] = useState({
    create: { uri: "", method: "GET", selectedRoles: [] },
    assign: { uri: "", method: "GET", selectedRoles: [] },
    deassign: { uri: "", method: "GET", selectedRoles: [] },
  });

  useEffect(() => {
    fetchRoles(setRoles, token);
    loadObjects(currentPage);
  }, [token, currentPage]);

  const loadObjects = async (page) => {
    try {
      const response = await fetchPaginatedObjects(page, pageSize, token);
      setObjects(response.content);
      setTotalPages(response.totalPages);
    } catch (error) {
      console.error("Error fetching objects:", error);
    }
  };

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

  const buildPayload = (section) => ({
    object: {
      request_uri: formState[section].uri,
      request_method: formState[section].method,
    },
    roles: formState[section].selectedRoles.map((role) => ({
      role_id: role.value,
    })),
  });

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
      loadObjects(currentPage);
    } catch (error) {
      console.error(error);
      showAlert(error.message || `Failed to ${section} URI`, "danger");
    }
  };

  const roleOptions = roles.map((role) => ({
    value: role.role_id,
    label: role.role_name,
  }));

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
              onChange={(e) =>
                handleInputChange(section, "uri", e.target.value)
              }
              placeholder="/dms/example"
            />
          </FormGroup>
          <FormGroup>
            <Label>Method</Label>
            <Input
              type="select"
              value={formState[section].method}
              onChange={(e) =>
                handleInputChange(section, "method", e.target.value)
              }
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

  const renderPagination = () => (
    <Pagination className="mt-3 justify-content-center">
      <PaginationItem disabled={currentPage === 0}>
        <PaginationLink first onClick={() => setCurrentPage(0)} />
      </PaginationItem>
      <PaginationItem disabled={currentPage === 0}>
        <PaginationLink
          previous
          onClick={() => setCurrentPage(currentPage - 1)}
        />
      </PaginationItem>
      {[...Array(totalPages)].map((_, index) => (
        <PaginationItem key={index} active={index === currentPage}>
          <PaginationLink onClick={() => setCurrentPage(index)}>
            {index + 1}
          </PaginationLink>
        </PaginationItem>
      ))}
      <PaginationItem disabled={currentPage === totalPages - 1}>
        <PaginationLink
          next
          onClick={() => setCurrentPage(currentPage + 1)}
        />
      </PaginationItem>
      <PaginationItem disabled={currentPage === totalPages - 1}>
        <PaginationLink
          last
          onClick={() => setCurrentPage(totalPages - 1)}
        />
      </PaginationItem>
    </Pagination>
  );

  return (
    <div>
      <Row className="gy-3">
        <Col md={4}>
          {renderForm(
            "create",
            (e) =>
              handleSubmit(
                e,
                "create",
                registerUriWithRoles,
                "URI created and roles assigned successfully!"
              ),
            "Create",
            "success"
          )}
        </Col>
        <Col md={4}>
          {renderForm(
            "assign",
            (e) =>
              handleSubmit(
                e,
                "assign",
                assignRolesToUri,
                "Roles assigned successfully!"
              ),
            "Assign",
            "primary"
          )}
        </Col>
        <Col md={4}>
          {renderForm(
            "deassign",
            (e) =>
              handleSubmit(
                e,
                "deassign",
                deassignRolesFromUri,
                "Roles de-assigned successfully!"
              ),
            "De-Assign",
            "danger"
          )}
        </Col>
      </Row>

      <Row className="mt-4">
        <Col>
          <Card className="shadow-sm">
            <CardBody>
              <h5 className="text-primary mb-3">Registered URIs</h5>
              {objects.length > 0 ? (
                <>
                  <div className="table-responsive">
                    <table className="table table-bordered table-hover">
                      <thead className="table-dark">
                        <tr>
                          <th>#</th>
                          <th>Method</th>
                          <th>URI</th>
                        </tr>
                      </thead>
                      <tbody>
                        {objects.map((obj, index) => (
                          <tr key={obj.omId || index}>
                            <td>{currentPage * pageSize + index + 1}</td>
                            <td>{obj.request_method}</td>
                            <td>{obj.request_uri}</td>
                          </tr>
                        ))}
                      </tbody>
                    </table>
                  </div>
                  {renderPagination()}
                </>
              ) : (
                <p className="text-muted">No objects found.</p>
              )}
            </CardBody>
          </Card>
        </Col>
      </Row>
    </div>
  );
};

export default UriManagement;
