import {
  Table,
  Form,
  Row,
  Col,
  Card,
  CardBody,
  FormGroup,
  Label,
  Input,
  Button,
} from "reactstrap";
import { FaPlus, FaUserMinus, FaTrash, FaEdit } from "react-icons/fa";
import { useState, useEffect } from "react";
import {
  fetchRoles,
  createRole,
  deleteRole,
  deassignRoleFromUser,
  assignRoleToUser,
} from "../../services/roleServices";
import { useAuth } from "../../context/AuthContext";
import { showAlert } from "../../utils/helpers";

const RoleManagement = ({ onRolesUpdate }) => {


//    Fullstack Java Developer Vijay Chaurasiya

  const [roles, setRoles] = useState([]);
  const [newRole, setNewRole] = useState("");
  const [userId, setUserId] = useState("");
  const [deassignRoleId, setDeassignRoleId] = useState("");
  const [assignUserId, setAssignUserId] = useState("");
  const [assignRoleId, setAssignRoleId] = useState("");
  const [loading, setLoading] = useState(true);
  const { token } = useAuth();

  const loadRoles = async () => {
    setLoading(true);
    try {
      // Call fetchRoles without pagination
      await fetchRoles(setRoles, token);
    } catch (error) {
      showAlert(error.response.data.message, error);
    } finally {
      setLoading(false);
    }
  };

  useEffect(() => {
    if (token) {
      loadRoles();
    }
  }, [token]);

  useEffect(() => {
    if (roles.length > 0) {
      onRolesUpdate(roles);
    }
  }, [roles, onRolesUpdate]);

const handleCreateRole = async () => {
  if (newRole.trim()) {
    try {
      const response = await createRole(newRole, token);
      console.log(response)
      
        showAlert(response.message, "success");
        setNewRole("");
        await loadRoles(); // reload roles after creation
      
      }
    catch(error) {
console.log(error)
console.log(error)
        showAlert(error.response.data.message || "Failed to create role.", "error");

    }
  }
};


  //  Deassign role
 const handleDeassignRole = async () => {
  console.log(userId + deassignRoleId);
  if (!userId || !deassignRoleId) {
    showAlert("Please provide both User ID and Role ID.");
    return;
  }

  try {
    const response = await deassignRoleFromUser(userId, deassignRoleId, token);
    // Use API response message on success
    showAlert(response.data.message || "Role deassigned successfully.", "success");
    setUserId("");
    setDeassignRoleId("");
  } catch (error) {
    console.error("Error deassigning role:", error);
    // Use backend error message if available
    const apiErrorMessage = error?.response?.data?.message || "Failed to deassign role.";
    showAlert(apiErrorMessage, "error");
  }
};

// ASSIGN ROLE TO USER
  const handleAssignRole = async () => {
    if (assignUserId && assignRoleId) {
      try {
       var res= await assignRoleToUser(assignUserId, assignRoleId, token); // <- pass token here
        showAlert(res.message,"success");
        setAssignUserId("");
        setAssignRoleId("");
      } catch (error) {
        console.error("Assign Role Error:", error);
        showAlert(error.response.data.message);
      }
    } else {
      showAlert("Please fill all fields.");
    }
  };

  return (
    <div>
      {/* Forms Row */}
      <Row className="gy-3">
        {/* Create Role */}
        <Col md={4}>
          <Card className="shadow-sm bg-light-subtle h-100">
            <CardBody>
              <h6 className="text-success mb-3">
                <FaPlus className="me-2" />
                Create New Role
              </h6>
              <Form
                onSubmit={(e) => {
                  e.preventDefault();
                  handleCreateRole();
                }}
              >
                <FormGroup>
                  <Label for="roleName">Role Name</Label>
                  <Input
                    id="roleName"
                    value={newRole}
                    onChange={(e) => setNewRole(e.target.value)}
                    placeholder="e.g., role_admin"
                  />
                </FormGroup>
                <Button color="success" type="submit" className="w-100">
                  Create Role
                </Button>
              </Form>
            </CardBody>
          </Card>
        </Col>

        {/* Assign Role */}
        <Col md={4}>
          <Card className="shadow-sm bg-light-subtle h-100">
            <CardBody>
              <h6 className="text-success mb-3">
                <FaPlus className="me-2" />
                Assign Role to User
              </h6>
              <Form
                onSubmit={(e) => {
                  e.preventDefault();
                  handleAssignRole();
                }}
              >
                <FormGroup>
                  <Label for="assignUserId">User ID</Label>
                  <Input
                    id="assignUserId"
                    type="number"
                    placeholder="Enter User ID"
                    value={assignUserId}
                    onChange={(e) => setAssignUserId(e.target.value)}
                  />
                </FormGroup>
                <FormGroup>
                  <Label for="assignRoleSelect">Select Role</Label>
                  <Input
                    id="assignRoleSelect"
                    type="select"
                    value={assignRoleId}
                    onChange={(e) => setAssignRoleId(e.target.value)}
                  >
                    <option value="">Select a Role</option>
                    {roles.map((role) => (
                      <option key={role.role_id} value={role.role_id}>
                        {role.role_name.replace("ROLE_", "")}
                      </option>
                    ))}
                  </Input>
                </FormGroup>
                <Button color="success" type="submit" className="w-100">
                  Assign Role
                </Button>
              </Form>
            </CardBody>
          </Card>
        </Col>

        {/* Deassign Role */}
        <Col md={4}>
          <Card className="shadow-sm bg-light-subtle h-100">
            <CardBody>
              <h6 className="text-danger mb-3">
                <FaUserMinus className="me-2" />
                De-assign Role from User
              </h6>
              <Form
                onSubmit={(e) => {
                  e.preventDefault();
                  handleDeassignRole();
                }}
              >
                <FormGroup>
                  <Label for="userId">User ID</Label>
                  <Input
                    id="userId"
                    type="number"
                    placeholder="Enter User ID"
                    value={userId}
                    onChange={(e) => setUserId(e.target.value)}
                  />
                </FormGroup>
                <FormGroup>
                  <Label for="roleSelect">Select Role</Label>
                  <Input
                    id="roleSelect"
                    type="select"
                    value={deassignRoleId}
                    onChange={(e) => setDeassignRoleId(e.target.value)}
                  >
                    <option value="">Select a Role</option>
                    {roles.map((role) => (
                      <option key={role.role_id} value={role.role_id}>
                        {role.role_name.replace("ROLE_", "")}
                      </option>
                    ))}
                  </Input>
                </FormGroup>
                <Button color="danger" type="submit" className="w-100">
                  De-assign Role
                </Button>
              </Form>
            </CardBody>
          </Card>
        </Col>
      </Row>

      {/* Roles Table */}
      {/* <Table responsive bordered hover className="mt-4">
        <thead className="table-dark">
          <tr>
            <th>#</th>
            <th>Role_Name</th>
            <th>Action</th>
          </tr>
        </thead>
        <tbody>
          {loading ? (
            <tr>
              <td colSpan="3" className="text-center">
                Loading...
              </td>
            </tr>
          ) : roles.length > 0 ? (
            roles.map((role) => (
              <tr key={role.roleId}>
                <td>{role.roleId}</td>
                <td>{role.role_name}</td>
                <td>
                  <Button
                    color="warning"
                    size="sm"
                    className="me-2"
                    title="Update Role"
                  >
                    <FaEdit />
                  </Button>
                  <Button
                    color="danger"
                    size="sm"
                    onClick={() => handleDeleteRole(role.role_id)}
                    title="Delete Role"
                  >
                    <FaTrash />
                  </Button>
                </td>
              </tr>
            ))
          ) : (
            <tr>
              <td colSpan="3" className="text-center">
                No roles found
              </td>
            </tr>
          )}
        </tbody>
      </Table> */}
    </div>
  );
};

export default RoleManagement;
