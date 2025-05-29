// src/pages/CaseFile/UploadMedia.js

import React, { useState } from "react";
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
  const [search, setSearch] = useState({
    caseType: "",
    caseNo: "",
    caseYear: "",
  });

  const toggleModal = () => setModal(!modal);

  const handleChange = (e) => {
    setSearch({ ...search, [e.target.name]: e.target.value });
  };

  const sampleCaseFiles = [
    { id: 1, caseType: "Civil", caseNo: "12345", caseYear: "2023" },
    { id: 2, caseType: "Criminal", caseNo: "98765", caseYear: "2022" },
  ];

  return (
    <div className="d-flex">
      <Sidebar />
      <div className="flex-grow-1">
        <Navbar />

        <Container fluid className="mt-3">
          <Row>
            <Col xs="12">
              <Card  inverse>
                <CardHeader className="d-flex justify-content-between align-items-center">
                  <h5 className="mb-0 text-dark">View Case File Details</h5>
                  <Button color="light" size="sm">
                    <i className="fa fa-expand" />
                  </Button>
                </CardHeader>

                <CardBody>
                  {/* Filter/Search Table */}
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
                            <option value="Civil">Civil</option>
                            <option value="Criminal">Criminal</option>
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

                  {/* Case File Results Table */}
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

          {/* Modal for Upload Media */}
          <Modal isOpen={modal} toggle={toggleModal} size="lg">
            <ModalHeader toggle={toggleModal}>
              <strong>Upload Media</strong>
            </ModalHeader>
            <ModalBody>
              {/* You can import an actual form component here */}
              <FormGroup>
                <Label for="mediaFile">Select Media File</Label>
                <Input type="file" id="mediaFile" />
              </FormGroup>
              <Button color="primary">Upload</Button>
            </ModalBody>
          </Modal>
        </Container>
      </div>
    </div>
  );
};

export default UploadMedia;
