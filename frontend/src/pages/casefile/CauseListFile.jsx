import React, { useState, useEffect } from "react";
import {
  Container,
  Row,
  Col,
  Button,
  Input,
  Card,
  CardHeader,
  CardBody,
  Table,
  FormGroup,
  Form,
  Label,
} from "reactstrap";
import DatePicker from "react-datepicker";
import "react-datepicker/dist/react-datepicker.css";
import Navbar from "../../components/layout/Navbar";
import Sidebar from "../../components/layout/Sidebar";
import {
  fetchCauseListTypes,
  fetchCourtMasterTypes,
  searchCauseLists,
} from "../../services/caseTypeService";
import { useAuth } from "../../context/AuthContext";

const CauseListFile = () => {
  const { token } = useAuth();

  const [causeListTypes, setCauseListTypes] = useState([]);
  const [courtMasterTypes, setCourtMasterTypes] = useState([]);
  const [selectedListType, setSelectedListType] = useState("");
  const [selectedCourtType, setSelectedCourtType] = useState("");
  const [selectedDate, setSelectedDate] = useState(new Date());

  const [causeListData, setCauseListData] = useState([]);

  useEffect(() => {
    if (token) {
      fetchCauseListTypes(token)
        .then((data) => setCauseListTypes(data))
        .catch((err) => console.error("Error fetching cause list types:", err));

      fetchCourtMasterTypes(token)
        .then((data) => setCourtMasterTypes(data))
        .catch((err) =>
          console.error("Error fetching court master types:", err)
        );
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

      const data = await searchCauseLists(
        selectedCourtType,
        selectedListType,
        formattedDate,
        token
      );

      console.log("List Type ID:", selectedListType);
      console.log("Court Type ID:", selectedCourtType);
      console.log("Date:", selectedDate.toISOString().split("T")[0]);

      setCauseListData(data);
      console.log(causeListData);
    } catch (error) {
      console.log("error" + error);
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
                        <option key={court.cm_id} value={court.cm_id}>
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

                <Col
                  md="3"
                  className="d-flex justify-content-md-end gap-2 flex-wrap"
                >
                  <Button color="primary" size="sm" onClick={handleSearch}>
                    Search
                  </Button>
                  <Button color="info" size="sm" className="text-white">
                    Download
                  </Button>
                </Col>
              </Row>

              {/* Action Buttons */}
             <Row className="mb-3">
  <Col>
    <Form className="d-flex gap-4 flex-wrap">
      <FormGroup check inline>
        <Input type="radio" name="radioOptions" value="option1" />
        <Label check>e-file</Label>
      </FormGroup>

      <FormGroup check inline>
        <Input type="radio" name="radioOptions" value="option2" />
        <Label check>all</Label>
      </FormGroup>

      <FormGroup check inline>
        <Input type="radio" name="radioOptions" value="option3" />
        <Label check>Digitized</Label>
      </FormGroup>
    </Form>
  </Col>
  <Col>
      <h3>Fresh Case</h3>
  </Col>
</Row>

              {/* Table */}
              <div className="table-responsive">
                <div className="table-responsive">
                  <Table
                    bordered
                    hover
                    striped
                    size="sm"
                    className="text-center align-middle cause-list-table"
                  >
                    <thead className="table-light ">
                      <tr>
                        <th>Sl No.</th>
                        <th>Cause List Type</th>
                        <th>Case Detail</th>
                        <th>Party Name</th>
                        <th>Petitioner's Counsel</th>
                        <th>Respondent Counsel</th>
                        <th>Court Name</th>
                        <th>Action</th>
                      </tr>
                    </thead>
                    <tbody>
                      {console.log(causeListData)}
                      {causeListData.map((causeList, index) => (
                        <tr key={causeList.cl_id}>
                          <td>{index + 1}</td>
                          <td>{causeList.clType?.clt_description}</td>
                          <td>
                            <a className="text-decoration-none text-dark fw-semibold">
                              {causeList.caseType?.label}
                              <br />
                              {causeList.cl_case_no}/{causeList.cl_case_year}
                            </a>
                          </td>
                          <td>
                            {causeList.cl_first_petitioner} <strong>Vs</strong>{" "}
                            {causeList.cl_first_respondent}
                          </td>
                          <td>{causeList.cl_petitioner_council}</td>
                          <td>{causeList.cl_respondent_council}</td>
                          <td>{causeList.courtMaster?.cm_name}</td>

                          <td>
                            <Button color="info" size="sm" outline>
                              View
                            </Button>
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
