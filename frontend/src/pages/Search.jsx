import React from 'react';
import { Container, Row, Col, Form, FormGroup, Input, Button, Table } from 'reactstrap';
import Sidebar from "../components/layout/Sidebar"; // Ensure correct path
import Navbar from "../components/layout/Navbar";

const Search = () => {
  return (
    <div className="d-flex">
      <Sidebar />
      <div className="flex-grow-1">
        <Navbar />

        <Container fluid className="p-4">
          {/* Header Section */}
          <div className="bg-dark py-2 rounded mb-4">
            <h5 className="text-light mt-2 mb-2 px-3">Search Cause List</h5>
          </div>

          {/* Search Form */}
          <Form>
            <Row className="g-3 align-items-center">
              {/* Select List Type */}
              <Col md="3" sm="6">
                <FormGroup>
                  <Input type="select">
                    <option value="">Select List Type...</option>
                    <option value="civil">Civil</option>
                    <option value="criminal">Criminal</option>
                    <option value="family">Family</option>
                  </Input>
                </FormGroup>
              </Col>

              {/* Transferred Checkbox */}
              <Col md="3" sm="6" className="d-flex align-items-center">
                <FormGroup check>
                  <Input className="form-check-input" type="checkbox" id="caseCheck" />
                  <label className="form-check-label" htmlFor="caseCheck">
                    Transferred
                  </label>
                </FormGroup>
              </Col>

              {/* Date Picker */}
              <Col md="3" sm="6">
                <FormGroup>
                  <Input type="date" id="dateInput" className="form-control" />
                </FormGroup>
              </Col>

              {/* Submit Button */}
              <Col md="3" sm="6" className="d-flex justify-content-md-end">
                <Button color="success" size="sm" className="mt-3">Submit</Button>
              </Col>
            </Row>
          </Form>

          {/* Table Section */}
          <div className="table-responsive mt-3 rounded">
            <Table bordered striped responsive size="sm" className="text-center align-middle">
              <thead className="table-secondary rounded">
                <tr>
                  <th>Sr No</th>
                  <th>Case No</th>
                  <th>Petitioners vs Responders</th>
                  <th>Petitioner Council</th>
                  <th>Respondent Council</th>
                </tr>
              </thead>
              <tbody>
                <tr>
                  <td>1</td>
                  <td>232</td>
                  <td>Lorem ipsum dolor sit amet consectetur adipisicing elit. Quod, suscipit.</td>
                  <td>Lorem ipsum dolor sit, amet consectetur adipisicing elit.</td>
                  <td>Lorem ipsum dolor sit amet.</td>
                </tr>
              </tbody>
            </Table>
          </div>
        </Container>
      </div>
    </div>
  );
};

export default Search;
