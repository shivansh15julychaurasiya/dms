import { useEffect, useState } from "react";
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
} from "reactstrap";
import Navbar from "../../components/layout/Navbar";
import Sidebar from "../../components/layout/Sidebar";
import {
  fetchCaseTypes,
  searchDocuments,
} from "../../services/caseTypeService";
import { useAuth } from "../../context/AuthContext";
import { useNavigate } from "react-router-dom";

const CaseFileView = () => {
  const navigate = useNavigate();

  const { token } = useAuth();
  const userRole = "DMSAdmin";

  const [caseTypes, setCaseTypes] = useState([]);
  const [searchResults, setSearchResults] = useState([]);
  const [search, setSearch] = useState({
    caseType: "",
    caseNo: "",
    caseYear: "",
  });

  const [showLoader, setShowLoader] = useState(false);
  const [showStatus, setShowStatus] = useState(false);

  useEffect(() => {
    if (!token) return;
    const loadCaseTypes = async () => {
      try {
        const data = await fetchCaseTypes(token);
        setCaseTypes(data || []);
      } catch (err) {
        console.error("Failed to load case types", err);
      }
    };
    loadCaseTypes();
  }, [token]);

  const handleChange = (e) => {
    setSearch({ ...search, [e.target.name]: e.target.value });
  };

  const handleSearch = async () => {
    if (!search.caseType || !search.caseNo || !search.caseYear) {
      alert("All fields are required");
      return;
    }

    setShowLoader(true);
    try {
      const results = await searchDocuments(
        token,
        search.caseType,
        search.caseNo,
        search.caseYear
      );
      setSearchResults(results);
      setShowStatus(true);
    } catch (err) {
      console.error("Search failed", err);
      setSearchResults([]);
    } finally {
      setShowLoader(false);
    }
  };

  return (
    <div className="d-flex">
      <Sidebar />
      <div className="flex-grow-1">
        <Navbar />
        <div style={{ backgroundColor: "white", padding: "20px" }}>
          <Container fluid>
            <Row>
              <Col md={12}>
                <Card className="shadow-sm">
                  <CardHeader className="d-flex justify-content-between align-items-center">
                    <h5 className="mb-0">View Case File Details</h5>
                  </CardHeader>
                  <CardBody>
                    {/* 🔍 Search Form */}
                    <Table borderless responsive>
                      <thead>
                        <tr>
                          <td style={{ width: "55%" }}>
                            <Input
                              type="select"
                              name="caseType"
                              value={search.caseType}
                              onChange={handleChange}
                            >
                              <option value="">Select Case Type</option>
                              {caseTypes.map((type) => (
                                <option key={type.id} value={type.id}>
                                  {type.name}
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
                              placeholder="Case No"
                            />
                          </td>
                          <td>
                            <Input
                              type="text"
                              name="caseYear"
                              value={search.caseYear}
                              onChange={handleChange}
                              placeholder="Case Year"
                            />
                          </td>
                          <td>
                            <Button
                              color="primary"
                              size="sm"
                              onClick={handleSearch}
                            >
                              Search
                            </Button>
                          </td>
                        </tr>
                      </thead>
                    </Table>

                    {/* 📄 Results Table */}
                    <Table striped bordered responsive>
                      <thead>
                        <tr>
                          <th>Sr.No.</th>
                          <th>Case Type</th>
                          <th>Case No</th>
                          <th>Case Year</th>
                          {userRole !== "Viewer Person" && (
                            <th>Exist in Efiling</th>
                          )}
                          <th>Action</th>
                        </tr>
                      </thead>
                      <tbody>
                        {searchResults.length > 0
                          ? searchResults.map((row, index) => (
                              <tr key={index}>
                                <td>{index + 1}</td>
                                <td>{row.caseType?.name}</td>
                                <td>{row.fdCaseNo}</td>
                                <td>{row.fdCaseYear}</td>
                                {userRole !== "Viewer Person" && <td>✓</td>}
                                <td>
                                  <Button
                                    color="success"
                                    size="sm"
                                    onClick={() =>
                                      navigate(`/dms/view-pdf/${"LeaveApp"}`)
                                    }
                                  >
                                    View
                                  </Button>
                                </td>
                              </tr>
                            ))
                          : showStatus && (
                              <tr>
                                <td colSpan={6} className="text-center">
                                  No Record found!!
                                </td>
                              </tr>
                            )}
                      </tbody>
                    </Table>

                    {showLoader && <div style={{ height: 60 }}>Loading...</div>}
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
