import React from "react";
import { Container, Row, Col, Button, Input, Card, CardHeader, CardBody, Table, FormGroup, Label } from "reactstrap";
import Navbar from "./layout/Navbar";
import Sidebar from "./layout/Sidebar";

const ManageCauseList = () => {
  return (
    <div className="d-flex">
      <Sidebar />
      <div className="flex-grow-1">
        <Navbar />

        <Container fluid className="p-04">
          <Card className="shadow-sm">
            <CardHeader className="bg-dark text-white fw-semibold">
              Manage Cause List
            </CardHeader>

            <CardBody>
              {/* Filters Row */}
              <Row className="g-3 align-items-center mb-3">
                <Col md="3">
                  <FormGroup>
                    <Input type="select">
                      <option>Select List Type</option>
                    </Input>
                  </FormGroup>
                </Col>
                <Col md="3">
                  <FormGroup>
                    <Input type="select">
                      <option>Select Court</option>
                    </Input>
                  </FormGroup>
                </Col>
                <Col md="3">
                  <FormGroup>
                    <Input
                      type="text"
                      value="Tue May 04 2021 12:11:04 GMT+..."
                      disabled
                    />
                  </FormGroup>
                </Col>
                <Col md="3" className="d-flex justify-content-md-end gap-2 flex-wrap">
                  <Button color="primary" size="sm">Search</Button>
                  <Button color="info" size="sm" className="text-white">Download</Button>
                </Col>
              </Row>

              {/* Action Buttons */}
              <Row className="mb-3">
                <Col className="d-flex gap-2 flex-wrap">
                  <Button color="info" size="sm" className="text-white">Upload CauseList</Button>
                  <Button color="info" size="sm" className="text-white">Add CaseToCauseList</Button>
                  <Button color="info" size="sm" className="text-white">Add Manual Case</Button>
                  <Button color="info" size="sm" className="text-white">Update Court</Button>
                  <Button color="danger" size="sm">Delete All</Button>
                </Col>
              </Row>

              {/* Table */}
              <div className="table-responsive">
                <Table bordered responsive size="sm" className="text-center align-middle">
                  <thead className="table-light">
                    <tr>
                      <th>Sl No.</th>
                      <th>Case No</th>
                      <th>Petitioner vs Respondent</th>
                      <th>Petitioner Council</th>
                      <th>Respondent Council</th>
                      <th>Court No.</th>
                      <th>
                        Select All <Input type="checkbox" />
                      </th>
                      <th>Action</th>
                    </tr>
                  </thead>
                  <tbody>
                    <tr>
                      <td colSpan="8" className="text-muted">
                        No records found.
                      </td>
                    </tr>
                  </tbody>
                </Table>
              </div>
            </CardBody>
          </Card>
        </Container>
      </div>
    </div>
  );
};

export default ManageCauseList;
