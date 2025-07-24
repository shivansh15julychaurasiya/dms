import { useState, useEffect } from "react";
import {
  Container,
  Table,
  Button,
  Card,
  CardHeader,
  CardBody,
  Modal,
  ModalHeader,
  ModalBody,
  ModalFooter,
  Form,
  FormGroup,
  Label,
  Input,
} from "reactstrap";
import Sidebar from "../../components/layout/Sidebar";
import Navbar from "../../components/layout/Navbar";
import { useAuth } from "../../context/AuthContext";
import { fetchCourtMasterTypes ,createCourtMasterType} from "../../services/caseTypeService";
import {showAlert} from "../../utils/helpers"


const ManageBenches = () => {
  const { token } = useAuth();

  const [courtMasterTypes, setCourtMasterTypes] = useState([]);
  const [modalOpen, setModalOpen] = useState(false);

  const [courtName, setCourtName] = useState("");
  const [status, setStatus] = useState("1");

  useEffect(() => {
    if (token) {
      fetchCourtMasterTypes(token)
        .then((data) => setCourtMasterTypes(data))
        .catch((err) =>
          console.error("Error fetching court master types:", err)
        );
    }
  }, [token]);

  const toggleModal = () => {
    setModalOpen(!modalOpen);
    setCourtName("");
    setStatus("1");
  };

const handleCreateNewCourt = async () => {
  try {
    // Extract numeric value from the court name (e.g., Court_16 → 16)
    const cmValue = courtName.split("_")[1];

    const newCourtData = {
      cm_name: courtName,
      cm_rec_status: status,
      cm_value: cmValue,
    };

    console.log("Submitted court data:", newCourtData);

    // Await API call to create court
    const created = await createCourtMasterType(newCourtData, token);
    showAlert("New Court Has Been Created ","success")
    console.log("Court created:", created);

    // Refresh court list
    const updated = await fetchCourtMasterTypes(token);
    setCourtMasterTypes(updated);

    // Close the modal after success
    toggleModal();
  } catch (error) {
    alert("Error creating court master: " + error.message);
  }
};


  return (
    <div className="d-flex">
      <Sidebar />
      <div className="flex-grow-1">
        <Navbar />

        <Container fluid className="mt-4">
          <Card>
            <CardHeader className="d-flex justify-content-between align-items-center bg-dark text-white">
              <h6 className="mb-0">Manage Benches</h6>
              <Button color="primary" size="sm" onClick={toggleModal}>
                <i className="fa fa-plus"></i> Add New Court
              </Button>
            </CardHeader>
            <CardBody>
              <Table bordered responsive hover>
                <thead className="thead-dark">
                  <tr>
                    <th>Court Name</th>
                    <th>Bench Id</th>
                    <th>Action</th>
                    <th>Service</th>
                  </tr>
                </thead>
                <tbody>
                  {courtMasterTypes.length === 0 ? (
                    <tr>
                      <td colSpan="4" className="text-center">
                        No courts available.
                      </td>
                    </tr>
                  ) : (
                    courtMasterTypes.map((court, index) => (
                      <tr key={index}>
                        <td>{court.cm_name}</td>
                        <td>{court.cm_bench_id || "-"}</td>
                        <td>
                          <Button color="info" size="sm">
                            Edit Bench
                          </Button>
                        </td>
                        <td className="d-flex gap-1 flex-wrap">
                          <Button color="success" size="sm">
                            Supplimentry
                          </Button>
                          <Button color="warning" size="sm">
                            Transfer
                          </Button>
                          <Button color="secondary" size="sm">
                            Correction&Mention
                          </Button>
                        </td>
                      </tr>
                    ))
                  )}
                </tbody>
              </Table>
            </CardBody>
          </Card>
        </Container>

        {/* Add Court Modal */}
        <Modal isOpen={modalOpen} toggle={toggleModal}>
          <ModalHeader toggle={toggleModal}>Add New Court</ModalHeader>
          <ModalBody>
            <Form>
              <FormGroup>
                <Label for="courtName">Court Name</Label>
                <Input
                  type="text"
                  id="courtName"
                  placeholder="Enter court name"
                  value={courtName}
                  onChange={(e) => setCourtName(e.target.value)}
                />
              </FormGroup>

              <FormGroup tag="fieldset">
                <Label>Status *</Label>
                <div className="d-flex">
                  <FormGroup check className="d-inline-block me-3">
                    <Input
                      type="radio"
                      name="status"
                      value="1"
                      checked={status === "1"}
                      onChange={(e) => setStatus(e.target.value)}
                    />
                    <Label check className="ms-1">
                      E-Court
                    </Label>
                  </FormGroup>
                  <FormGroup check className="d-inline-block">
                    <Input
                      type="radio"
                      name="status"
                      value="3"
                      checked={status === "3"} // fixed value from "3" to "2"
                      onChange={(e) => setStatus(e.target.value)}
                    />
                    <Label check className="ms-1">
                      In-Chamber
                    </Label>
                  </FormGroup>
                </div>
              </FormGroup>
            </Form>
          </ModalBody>
          <ModalFooter>
            <Button color="primary" onClick={handleCreateNewCourt}>
              Add
            </Button>
            <Button color="secondary" onClick={toggleModal}>
              Cancel
            </Button>
          </ModalFooter>
        </Modal>
      </div>
    </div>
  );
};

export default ManageBenches;
