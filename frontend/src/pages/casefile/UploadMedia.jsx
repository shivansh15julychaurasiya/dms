import React, { useEffect, useState } from "react";
import {
  Container,
  Row,
  Col,
  Table,
  Input,
  Button,
  Modal,
  ModalHeader,
  ModalBody,
  FormGroup,
  Label,
  Card,
  CardHeader,
  CardBody,
} from "reactstrap";
import Sidebar from "../../components/layout/Sidebar";
import Navbar from "../../components/layout/Navbar";


const UploadMedia = () => {



  const [modal, setModal] = useState(false);
  const [file, setFile] = useState(null);
  const [uploading, setUploading] = useState(false);

  const [search, setSearch] = useState({
    caseType: "",
    caseNo: "",
    caseYear: "",
  });

  const sampleCaseFiles = [
    { id: 1, caseType: "Civil", caseNo: "12345", caseYear: "2023" },
    { id: 2, caseType: "Criminal", caseNo: "98765", caseYear: "2022" },
  ];

  const toggleModal = () => {
    setModal(!modal);
    setFile(null); // Clear file input on modal close
  };

  const handleChange = (e) => {
    setSearch({ ...search, [e.target.name]: e.target.value });
  };



  // const handleUpload = async () => {
  //   if (!file) return alert("Please select a file.");
  //   if (!search.caseType || !search.caseNo || !search.caseYear) {
  //     return alert("Please fill in all case details.");
  //   }

  //   const formData = new FormData();
  //   formData.append("file", file);
  //   formData.append("caseType", search.caseType);
  //   formData.append("caseNo", search.caseNo);
  //   formData.append("caseYear", search.caseYear);

  //   try {
  //     setUploading(true);
  //     const token = localStorage.getItem("token");
  //     const response = await axiosInstance.post(
  //       "/api/documents/upload",
  //       formData,
  //       {
  //         headers: {
  //           Authorization: `Bearer ${token}`,
  //           "Content-Type": "multipart/form-data",
  //         },
  //       }
  //     );
  //     alert("File uploaded successfully!");
  //     toggleModal();
  //     setSearch({ caseType: "", caseNo: "", caseYear: "" });
  //   } catch (error) {
  //     console.error("Upload failed:", error);
  //     alert("Upload failed. Please try again.");
  //   } finally {
  //     setUploading(false);
  //   }
  // };

  return (
    <div className="d-flex">
      <Sidebar />
      <div className="flex-grow-1">
        <Navbar />

        <Container fluid className="mt-3">
          <Row>
            <Col xs="12">
              <Card inverse>
                <CardHeader className="d-flex justify-content-between align-items-center">
                  <h5 className="mb-0 text-dark">View Case File Details</h5>
                  <Button color="light" size="sm">
                    <i className="fa fa-expand" />
                  </Button>
                </CardHeader>

                <CardBody>
                  <Table bordered responsive>
                    <thead>
                      <tr>
                        <th style={{ textAlign: "center", width: "40%" }}>
                          Case Type
                        </th>
                        <th style={{ textAlign: "center", width: "20%" }}>
                          Case Number
                        </th>
                        <th style={{ textAlign: "center", width: "25%" }}>
                          Case Year
                        </th>
                        <th style={{ textAlign: "center", width: "15%" }}>
                          Action
                        </th>
                      </tr>
                      <tr>
                        <td>
                          <Input
                            type="select"
                            name="caseType"
                            value={search.caseType}
                            onChange={handleChange}
                          >
                            <option value="">Select Case Type</option>
                            {caseTypes.map((type) => (
                              <option key={type.id} value={type.label}>
                                {type.label}
                              </option>
                            ))}
                          </Input>
                        </td>
                        <td>
                          <Input
                            type="text"
                            name="caseNo"
                            value={search.caseNo}
                            onChange={handleChange}
                            placeholder="Enter Case No"
                          />
                        </td>
                        <td>
                          <Input
                            type="text"
                            name="caseYear"
                            value={search.caseYear}
                            onChange={handleChange}
                            placeholder="Enter Case Year"
                          />
                        </td>
                        <td>
                          <Button color="primary" size="sm">
                            Search
                          </Button>
                        </td>
                      </tr>
                    </thead>
                  </Table>

                  <Table bordered striped>
                    <thead>
                      <tr>
                        <th width="15%">Sr.No.</th>
                        <th width="40%">Case Type</th>
                        <th width="25%">Case No</th>
                        <th width="20%">Case Year</th>
                        <th>Action</th>
                      </tr>
                    </thead>
                    <tbody>
                      {sampleCaseFiles.length > 0 ? (
                        sampleCaseFiles.map((row, index) => (
                          <tr key={row.id}>
                            <td>{index + 1}</td>
                            <td>{row.caseType}</td>
                            <td>{row.caseNo}</td>
                            <td>{row.caseYear}</td>
                            <td>
                              <Button
                                color="success"
                                size="sm"
                                onClick={toggleModal}
                              >
                                Upload Media
                              </Button>
                            </td>
                          </tr>
                        ))
                      ) : (
                        <tr>
                          <td colSpan="5" className="text-center">
                            No Record found!!
                          </td>
                        </tr>
                      )}
                    </tbody>
                  </Table>
                </CardBody>
              </Card>
            </Col>
          </Row>

          {/* Upload Modal */}
          <Modal isOpen={modal} toggle={toggleModal} size="lg">
            <ModalHeader toggle={toggleModal}>
              <strong>Upload Media</strong>
            </ModalHeader>
            <ModalBody>
              <FormGroup>
                <Label for="mediaFile">Select Media File</Label>
                <Input
                  type="file"
                  id="mediaFile"
                  onChange={(e) => setFile(e.target.files[0])}
                />
              </FormGroup>
              <Button color="primary" onClick={handleUpload} disabled={uploading}>
                {uploading ? "Uploading..." : "Upload"}
              </Button>
            </ModalBody>
          </Modal>
        </Container>
      </div>
    </div>
  );
};

export default UploadMedia;
