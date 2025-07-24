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
import {
  fetchCourtMasterTypes,
  createCourtMasterType,
  updateCourtBenchId,
} from "../../services/caseTypeService";
import { showAlert } from "../../utils/helpers";

const ManageBenches = () => {
  const { token } = useAuth();

  const [courtMasterTypes, setCourtMasterTypes] = useState([]);
  const [modalOpen, setModalOpen] = useState(false);

  const [courtName, setCourtName] = useState("");
  const [status, setStatus] = useState("1");

  const [editIndex, setEditIndex] = useState(null);
  const [editedBenchId, setEditedBenchId] = useState("");
  const [showConfirmModal, setShowConfirmModal] = useState(false);
  const [selectedBenchIndex, setSelectedBenchIndex] = useState(null);

  useEffect(() => {
    if (token) {
      fetchCourtMasterTypes(token)
        .then((data) => setCourtMasterTypes(data))
        .catch((err) => console.error("Error fetching court master types:", err));
    }
  }, [token]);

  const toggleModal = () => {
    setModalOpen(!modalOpen);
    setCourtName("");
    setStatus("1");
  };

  const handleCreateNewCourt = async () => {
    try {
      const cmValue = courtName.split("_")[1];
      const newCourtData = {
        cm_name: courtName,
        cm_rec_status: status,
        cm_value: cmValue,
      };

      await createCourtMasterType(newCourtData, token);
      showAlert("New Court Has Been Created", "success");

      const updated = await fetchCourtMasterTypes(token);
      setCourtMasterTypes(updated);
      toggleModal();
    } catch (error) {
      alert("Error creating court master: " + error.message);
    }
  };

  const handleSaveBenchId = async (court) => {
    try {
      await updateCourtBenchId(court.cm_id, editedBenchId, token);
      const updated = await fetchCourtMasterTypes(token);
      setCourtMasterTypes(updated);
      setEditIndex(null);
      setEditedBenchId("");
      showAlert("Bench ID updated successfully", "success");
    } catch (error) {
      const backendMessage = error?.response?.data?.message || "Update failed!";
      showAlert(backendMessage);
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
                    <th>Bench ID</th>
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
                      <tr key={court.cm_id}>
                        <td>{court.cm_name}</td>
                        <td>
                          {editIndex === index ? (
                            <Input
                              type="number"
                              size="sm"
                              value={editedBenchId}
                              onChange={(e) => setEditedBenchId(e.target.value)}
                            />
                          ) : (
                            court.cm_bench_id || "-"
                          )}
                        </td>
                        <td>
                          {editIndex === index ? (
                            <>
                              <Button
                                color="success"
                                size="sm"
                                className="me-2"
                                onClick={() => handleSaveBenchId(court)}
                              >
                                Save
                              </Button>
                              <Button
                                color="secondary"
                                size="sm"
                                onClick={() => {
                                  setEditIndex(null);
                                  setEditedBenchId("");
                                }}
                              >
                                Cancel
                              </Button>
                            </>
                          ) : (
                            <Button
                              color="info"
                              size="sm"
                              onClick={() => {
                                setSelectedBenchIndex(index);
                                setShowConfirmModal(true);
                              }}
                            >
                              Edit Bench
                            </Button>
                          )}
                        </td>
                        <td className="d-flex gap-1 flex-wrap">
                          <Button color="success" size="sm">
                            Supplementary
                          </Button>
                          <Button color="warning" size="sm">
                            Transfer
                          </Button>
                          <Button color="secondary" size="sm">
                            Correction & Mention
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
                    <Label check className="ms-1">E-Court</Label>
                  </FormGroup>
                  <FormGroup check className="d-inline-block">
                    <Input
                      type="radio"
                      name="status"
                      value="3"
                      checked={status === "3"}
                      onChange={(e) => setStatus(e.target.value)}
                    />
                    <Label check className="ms-1">In-Chamber</Label>
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

        {/* Confirm Edit Modal */}
        <Modal
          isOpen={showConfirmModal}
          toggle={() => setShowConfirmModal(false)}
        >
          <ModalHeader toggle={() => setShowConfirmModal(false)}>
            Confirm Edit
          </ModalHeader>
          <ModalBody>
            Are you sure you want to edit the Bench ID for:{" "}
            <strong>{courtMasterTypes[selectedBenchIndex]?.cm_name}</strong>?
          </ModalBody>
          <ModalFooter>
            <Button
              color="primary"
              onClick={() => {
                const court = courtMasterTypes[selectedBenchIndex];
                setEditIndex(selectedBenchIndex);
                setEditedBenchId(court.cm_bench_id || "");
                setShowConfirmModal(false);
              }}
            >
              Yes
            </Button>
            <Button color="secondary" onClick={() => setShowConfirmModal(false)}>
              Cancel
            </Button>
          </ModalFooter>
        </Modal>
      </div>
    </div>
  );
};

export default ManageBenches;
