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
  Modal,
} from "reactstrap";
import { FaLink } from "react-icons/fa";
import Select from "react-select";
import { useAuth } from "../../context/AuthContext";
import { showAlert } from "../../utils/helpers";
import { toast } from "react-toastify";

import { fetchRoles } from "../../services/roleServices";
import {
  registerUriWithRoles,
  assignRolesToUri,
  deassignRolesFromUri,
  fetchPaginatedObjects,
  enableObjectUri,
  disableObjectUri,
} from "../../services/userService";
import { Switch } from "@mui/material";

const UriManagement = () => {

//    Fullstack Java Developer Vijay Chaurasiya

  const { token } = useAuth();

  const [roles, setRoles] = useState([]);
  const [objects, setObjects] = useState([]);
  const [currentPage, setCurrentPage] = useState(0);
  const [pageSize] = useState(5);
  const [totalPages, setTotalPages] = useState(0);

  const [showConfirmModal, setShowConfirmModal] = useState(false);
  const [pendingToggleObject, setPendingToggleObject] = useState(null);

  const [formState, setFormState] = useState({
    create: { uri: "", method: "GET", selectedRoles: [] },
    assign: { uri: "", method: "GET", selectedRoles: [] },
    deassign: { uri: "", method: "GET", selectedRoles: [] },
  });


  const uriOptions = objects.map((obj) => ({
  value: obj.request_uri,
  label: obj.request_uri,
}));

  useEffect(() => {
    fetchRoles(setRoles, token);
    loadObjects(currentPage);
  }, [token, currentPage]);

  const loadObjects = async (page) => {
    try {
      const response = await fetchPaginatedObjects(page, pageSize, token);
      const objs = response.content.map((obj) => ({
        ...obj,
        enabled: typeof obj.enabled === "boolean" ? obj.enabled : true,
      }));
      setObjects(objs);
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
      lk_id: role.value,
    })),
  });

  const handleSubmit = async (e, section, serviceMethod, successMsg) => {
    e.preventDefault();
    try {
      var res=null;
      const payload = buildPayload(section);
      console.log("Payload"+section)
     res= await serviceMethod(payload, token);
     console.log(res)
      showAlert(res.message, "success");
      setFormState((prev) => ({
        ...prev,
        [section]: { uri: "", method: "GET", selectedRoles: [] },
      }));
      await loadObjects(currentPage);
    } catch (error) {
      console.error(error);
      console.log(error.response.data.message)
      showAlert(error.response.data.message,"error");
    }
  };

  const toggleUriStatus = async (object) => {
    try {
      const isActive=object.status
      console.log(object.status)
      if (isActive) {
        await disableObjectUri(object.om_id, token);
        toast.info(`${object.request_uri} disabled successfully`);
      } else {
        await enableObjectUri(object.om_id, token);
        toast.success(`${object.request_uri} enabled successfully`);
      }

      setObjects((prevObjects) =>
        prevObjects.map((o) =>
          o.om_id === object.om_id ? { ...o, status: !isActive } : o
        )
      );
    } catch (error) {
      console.error("Toggle status error:", error);
      toast.error(
        error?.response?.data?.message || "Failed to toggle URI status"
      );
    }
  };

  const roleOptions = roles.map((role) => ({
    value: role.lk_id,
    label: role.lk_longname,
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
  {section === "create" ? (
    // CREATE: Text input
    <Input
      value={formState[section].uri}
      onChange={(e) => handleInputChange(section, "uri", e.target.value)}
      placeholder="/dms/example"
    />
  ) : (
    // ASSIGN / DEASSIGN: Dropdown
    <Select
      options={uriOptions}
      value={
        formState[section].uri
          ? { value: formState[section].uri, label: formState[section].uri }
          : null
      }
      onChange={(selected) => {
        handleInputChange(section, "uri", selected ? selected.value : "");
      }}
      placeholder="Select URI..."
      classNamePrefix="react-select"
    />
  )}
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
        <PaginationLink next onClick={() => setCurrentPage(currentPage + 1)} />
      </PaginationItem>
      <PaginationItem disabled={currentPage === totalPages - 1}>
        <PaginationLink last onClick={() => setCurrentPage(totalPages - 1)} />
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
                    <table className="table table-bordered table-hover align-middle">
                      <thead className="table-light text-center">
                        <tr>
                          <th>#</th>
                          <th>URI</th>
                          <th>Method</th>
                          <th>Enable/Disable</th>
                        </tr>
                      </thead>
                      <tbody>
                        {objects.map((obj, idx) => (
                          <tr key={obj.om_id}>
                            <td className="text-center">
                              {currentPage * pageSize + idx + 1}
                            </td>
                            <td>{obj.request_uri}</td>
                            <td className="text-center">
                              {obj.request_method}
                            </td>
                            <td className="text-center">
                              <Switch
                                checked={obj.status}
                                onChange={() => {
                                  setPendingToggleObject(obj);
                                  setShowConfirmModal(true);
                                }}
                                size="small"
                              />
                            </td>
                          </tr>
                        ))}
                      </tbody>
                    </table>
                  </div>
                  {renderPagination()}
                </>
              ) : (
                <p>No URIs found.</p>
              )}
            </CardBody>
          </Card>
        </Col>
      </Row>

      {/* Modal for Activation/Deactivation Confirmation */}
      <Modal
        isOpen={showConfirmModal}
        toggle={() => setShowConfirmModal(false)}
      >
        <div className="modal-header">
          <h5 className="modal-title">
            {pendingToggleObject?.enabled ? "Disable URI" : "Enable URI"}
          </h5>
          <button
            type="button"
            className="btn-close"
            onClick={() => setShowConfirmModal(false)}
          ></button>
        </div>
        <div className="modal-body">
          Are you sure you want to{" "}
          {pendingToggleObject?.enabled ? "disable" : "enable"} URI:{" "}
          <strong>{pendingToggleObject?.request_uri}</strong>?
        </div>
        <div className="modal-footer">
          <Button color="secondary" onClick={() => setShowConfirmModal(false)}>
            Cancel
          </Button>
          <Button
            color={pendingToggleObject?.enabled ? "danger" : "success"}
            onClick={() => {
              toggleUriStatus(pendingToggleObject);
              setShowConfirmModal(false);
              setPendingToggleObject(null);
            }}
          >
            Yes, {pendingToggleObject?.enabled ? "Disable" : "Enable"}
          </Button>
        </div>
      </Modal>
    </div>
  );
};

export default UriManagement;
