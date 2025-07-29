import React, { useState, useEffect } from "react";
import {
  Container, Row, Col, Button, Input, Card, CardHeader, CardBody,
  Table, FormGroup, Label
} from "reactstrap";
import DatePicker from "react-datepicker";
import "react-datepicker/dist/react-datepicker.css";
import Navbar from "../../components/layout/Navbar";
import Sidebar from "../../components/layout/Sidebar";
import { fetchCauseListTypes, fetchCourtMasterTypes,searchCauseLists} from "../../services/caseTypeService";
import { useAuth } from "../../context/AuthContext";

const CauseListFile = () => {
  const { token } = useAuth();

  const [causeListTypes, setCauseListTypes] = useState([]);
  const [courtMasterTypes, setCourtMasterTypes] = useState([]);
  const [selectedListType, setSelectedListType] = useState("");
  const [selectedCourtType, setSelectedCourtType] = useState("");
  const [selectedDate, setSelectedDate] = useState(new Date());

  const [causeListData ,setCauseListData]=useState([]);

  useEffect(() => {
    if (token) {
      fetchCauseListTypes(token)
        .then(data => setCauseListTypes(data))
        .catch(err => console.error("Error fetching cause list types:", err));

      fetchCourtMasterTypes(token)
        .then(data => setCourtMasterTypes(data))
        .catch(err => console.error("Error fetching court master types:", err));
    }
  }, [token]);

  // const handleSearch = () => {
  //   console.log("List Type ID:", selectedListType);
  //   console.log("Court Type ID:", selectedCourtType);
  //   console.log("Date:", selectedDate.toISOString().split("T")[0]);


  //   // Add your API call here
  // };
  const handleSearch = async () => {

  const formattedDate = selectedDate.toISOString().split("T")[0];

  try {
      // console.log(token)

    const data = await searchCauseLists( selectedCourtType, selectedListType ,  formattedDate ,token );

    console.log("List Type ID:", selectedListType);
    console.log("Court Type ID:", selectedCourtType);
    console.log("Date:", selectedDate.toISOString().split("T")[0]);

    setCauseListData(data);
    console.log(causeListTypes)
  } catch (error) {
   console.log("error"+error)
  }
};


  return (
    <div className="d-flex">
      <Sidebar />
      <div className="flex-grow-1">
        <Navbar />
        <Container fluid className="p-4">
          <Card className="shadow-sm">
            <CardHeader className="bg-dark text-white fw-semibold">
              Manage Cause List
            </CardHeader>
            <CardBody>
              {/* Filters Row */}
              <Row className="g-3 align-items-center mb-3">
                <Col md="3">
                  <FormGroup>
                    <Label for="listType">Select Cause List Type</Label>
                    <Input
                      id="listType"
                      type="select"
                      value={selectedListType}
                      onChange={(e) => setSelectedListType(e.target.value)}
                    >
                      <option value="">Select List Type</option>
                      {causeListTypes.map((type) => (
                        <option key={type.clt_id} value={type.clt_id}>
                          {type.clt_description}
                        </option>
                      ))}
                    </Input>
                  </FormGroup>
                </Col>

                <Col md="3">
                  <FormGroup>
                    <Label for="courtType">Select Court</Label>
                    <Input
                      id="courtType"
                      type="select"
                      value={selectedCourtType}
                      onChange={(e) => setSelectedCourtType(e.target.value)}
                    >
                      <option value="">Select Court</option>
                      {courtMasterTypes.map((court) => (
                        <option key={court.cm_id
                        } value={court.cm_id}>
                          {court.cm_name}
                        </option>
                      ))}
                    </Input>
                  </FormGroup>
                </Col>

                <Col md="3">
                  <FormGroup>
                    <Label>Select Date</Label>
                    <DatePicker
                      selected={selectedDate}
                      onChange={(date) => setSelectedDate(date)}
                      className="form-control"
                      dateFormat="dd-MM-yyyy"
                    />
                  </FormGroup>
                </Col>

                <Col md="3" className="d-flex justify-content-md-end gap-2 flex-wrap">
                  <Button color="primary" size="sm" onClick={handleSearch}>
                    Search
                  </Button>
                  <Button color="info" size="sm" className="text-white">
                    Download
                  </Button>
                </Col>
              </Row>

              {/* Action Buttons */}
              {/* <Row className="mb-3">
                <Col className="d-flex gap-2 flex-wrap">
                  <Button color="info" size="sm" className="text-white">Upload CauseList</Button>
                  <Button color="info" size="sm" className="text-white">Add CaseToCauseList</Button>
                  <Button color="info" size="sm" className="text-white">Add Manual Case</Button>
                  <Button color="info" size="sm" className="text-white">Update Court</Button>
                  <Button color="danger" size="sm">Delete All</Button>
                </Col>
              </Row> */}

              {/* Table */}
              <div className="table-responsive">
                <div className="table-responsive">
  <Table bordered responsive size="sm" className="text-center align-middle">
    <thead className="table-light">
      <tr>
        <th>Sl No.</th>
        <th>Cause List Type</th>
        <th>Case Detail</th>
        <th>Party Name</th>
        <th>Petitioner's Counsel</th>
        <th>Respondent Counsel</th>
        <th>Court Name</th>
        <th>Status</th>
        <th>Action</th>
      </tr>
    </thead>
   <tbody>
  {causeListData.map((causeList, index) => (
    <tr key={causeList.cl_id}>
      <td>{index + 1}</td>
      <td>{causeList.clType.clt_description}</td>
      <td>
       <a>{causeList.caseType.label}<br/>
        {causeList.cl_case_no}/
        {causeList.cl_case_year}<br/></a>
      </td>
      <td>{causeList.cl_first_petitioner} Vs {causeList.cl_first_respondent}</td>
      <td>{causeList.cl_petitioner_council}</td>
      <td>{causeList.cl_respondent_council}</td>
      <td>{causeList.courtMaster.cm_name}</td>
      <td>Active</td> {/* If you have status info, replace "Active" dynamically */}
      <td>
        <Button color="primary" size="sm">View</Button>
      </td>
    </tr>
  ))}
</tbody>

  </Table>
</div>

              </div>
            </CardBody>
          </Card>
        </Container>
      </div>
    </div>
  );
};

export default CauseListFile;
