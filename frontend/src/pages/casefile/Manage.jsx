import { useState, useEffect } from 'react';
import {
  Container,
  Row,
  Col,
  Card,
  CardHeader,
  CardBody,
  Table,
  Button,
  Input,
  FormGroup,

} from 'reactstrap';
import Navbar from '../../components/layout/Navbar';
import Sidebar from '../../components/layout/Sidebar';
import { fetchCaseTypes } from '../../services/courtManage';
import { useAuth } from '../../context/AuthContext';




const CaseFileView = () => {
  const { token } = useAuth();

  const userRole = 'DMSAdmin'; // dynamically set based on auth
  const caseFileList = []; // replace with actual case files
  const officeReport = []; // replace with actual report list
  const showLoader = false;
  const showStatus = false;
  const [caseTypes, setCaseTypes] = useState([]);
  


  useEffect(() => {
    if (token) {
      fetchCaseTypes(token)
        .then((res) => setCaseTypes(res.data))
        .catch((error) => console.log(error))
    }
  }, [])
   useEffect(() => {
    console.log(caseTypes)
  }, [])

  return (

    <div className="d-flex">
      <Sidebar />
      <div className="flex-grow-1">
        <Navbar />

        <div style={{ backgroundColor: 'white', padding: '20px' }}>
          <Container fluid>
            <Row>
              <Col md={12}>
                <Card className="shadow-sm">
                  <CardHeader className="d-flex justify-content-between align-items-center">
                    <h5 className="mb-0">View Case File Details</h5>
                  </CardHeader>
                  <CardBody>
                    {/* Search Form */}
                    <Table borderless responsive>
                      <thead>
                        <tr>
                          <td style={{ width: '55%' }}>
                            <Input type="select">
                              <option value="">Select Case Type</option>
                              {/* Map options dynamically */}
                              {caseTypes.map((casetype) => (
                                <option key={casetype.cm_id} value={casetype.cm_id}>
                                  {casetype.ct_label} - {casetype.ct_name}
                                </option>
                              ))}
                            </Input>
                          </td>
                          <td>
                            <Input type="text" placeholder="Case No" />
                          </td>
                          <td>
                            <Input type="text" placeholder="Case Year" />
                          </td>
                          <td>
                            <Button color="primary" size="sm">Search</Button>
                          </td>
                        </tr>
                      </thead>
                    </Table>

                    {/* Case File Table */}
                    <Table striped bordered responsive>
                      <thead>
                        <tr>
                          <th>Sr.No.</th>
                          <th>Case Type</th>
                          <th>Case No</th>
                          <th>Case Year</th>
                          {userRole !== 'Viewer Person' && <th>Exist in Efiling</th>}
                          <th>Action</th>
                        </tr>
                      </thead>
                      <tbody>
                        {caseFileList.length > 0 ? (
                          caseFileList.map((row, index) => (
                            <tr key={index}>
                              <td>{index + 1}</td>
                              <td>{row.caseType?.ct_name}</td>
                              <td>{row.fd_case_no}</td>
                              <td>{row.fd_case_year}</td>
                              {userRole !== 'Viewer Person' && <td>{row.status}</td>}
                              <td>
                                <Button color="success" size="sm" className="me-1">View</Button>
                                {userRole === 'DMSAdmin' && (
                                  <>
                                    <Button color="success" size="sm" className="me-1">View Detail</Button>
                                    <Button color="success" size="sm" className="me-1">Generate Decree</Button>
                                    <Button color="success" size="sm" className="me-1">View Decree</Button>
                                    <Button color="success" size="sm" className="me-1">Download</Button>
                                    <Button color="success" size="sm" className="me-1">Change Case Type</Button>
                                    <Button color="success" size="sm" className="me-1">Upload</Button>
                                    <Button color="success" size="sm" className="me-1">Office Reports</Button>
                                    <Button color="success" size="sm" className="me-1">Assign To</Button>
                                    <Button color="success" size="sm" className="me-1">AddCaseToEfiling</Button>
                                  </>
                                )}
                              </td>
                            </tr>
                          ))
                        ) : (
                          showStatus && (
                            <tr>
                              <td colSpan={6} className="text-center">No Record found!!</td>
                            </tr>
                          )
                        )}
                      </tbody>
                    </Table>

                    {/* Loader */}
                    {showLoader && <div style={{ height: 60 }}>Loading...</div>}

                    {/* Office Report */}
                    {officeReport.length > 0 && (
                      <Table striped bordered responsive>
                        <thead>
                          <tr>
                            <th style={{ width: '10%' }}>Date</th>
                            <th style={{ width: '70%' }}>Report</th>
                            <th style={{ width: '20%' }}>Action</th>
                          </tr>
                        </thead>
                        <tbody>
                          {officeReport.map((data, idx) => (
                            <tr key={idx}>
                              <td>{new Date(data.ord_created).toLocaleString()}</td>
                              <td>{data.ord_remark}</td>
                              <td>
                                {data.cl_flag === 0 ? (
                                  <Button color="success" size="sm">Edit Report</Button>
                                ) : data.cl_flag === 1 ? (
                                  <p>Case is Listed In Court</p>
                                ) : (
                                  <p>Time Expired</p>
                                )}
                              </td>
                            </tr>
                          ))}
                        </tbody>
                      </Table>
                    )}


                  </CardBody>
                </Card>
              </Col>
            </Row>
          </Container>
        </div>

      </div>
    </div>
  );
};

export default CaseFileView;
