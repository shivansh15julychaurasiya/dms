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
// import { useNavigate } from "react-router-dom";
// import { useSearchResult } from "../../context/CaseFileDetailsSearchContextProvider";
// import { fetchDocumentFileById } from "../../services/PdfFileService";

const CaseFileView = () => {

//    Fullstack Java Developer Vijay Chaurasiya

  // const { setSearchResult } = useSearchResult();

  // const navigate = useNavigate();

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
      console.log(search.caseType+" "+ search.caseNo+search.caseYear)
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
                    {/*  Search Form */}
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
                                  {type.label}-{type.name}
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

                    {/*  Results Table */}
             <Table striped bo  rdered responsive size="sm" className="table-sm" style={{ fontSize: "0.8rem" }}>
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
                                {/* <td>{row.subDocument?.[0].sd_document_name}</td> */}

                                {userRole !== "Viewer Person" && <td>✓</td>}
                                <td>
                                  <Button
                                    color="success"
                                    size="sm"
                                     className="me-1"
                                    onClick={() =>
                                      window.open(`/dms/viewer?id=1`, "_blank")
                                    }
                                  >
                                    View
                                  </Button>

                                  {/* <Button
                                    color="success"
                                    size="sm"
                                    onClick={() => {
                                      localStorage.setItem(
                                        "selectedCaseFile",
                                        JSON.stringify(row)
                                      );
                                      localStorage.setItem(
                                        "documentName",
                                        row.subDocument[0].sd_document_name
                                      );
                                      window.open("/dms/viewer", "_blank"); //  opens in new tab
                                    }}
                                  >
                                    View
                                  </Button> */}

                                  <Button
                                    color="success"
                                    size="sm"
                                    className="me-1"
                                    onClick={() =>
                                      window.open(`/dms/viewer?id=1`, "_blank")
                                    }
                                  >
                                    View Details
                                  </Button>

                                  <Button
                                    color="primary"
                                    size="sm"
                                    className="me-1"
                                    onClick={() =>
                                      console.log("Download clicked")
                                    }
                                  >
                                    Download
                                  </Button>

                                  <Button
                                    color="warning"
                                    size="sm"
                                    className="me-1"
                                    onClick={() =>
                                      console.log("Change Case Type clicked")
                                    }
                                  >
                                    Change Case Type
                                  </Button>

                                  <Button
                                    color="info"
                                    size="sm"
                                    className="me-1"
                                    onClick={() =>
                                      console.log("Upload clicked")
                                    }
                                  >
                                    Upload
                                  </Button>

                                  <Button
                                    color="secondary"
                                    size="sm"
                                    className="me-1"
                                    onClick={() =>
                                      console.log("Office Report clicked")
                                    }
                                  >
                                    Office Report
                                  </Button>

                                  <Button
                                    color="dark"
                                    size="sm"
                                    className="me-1"
                                    onClick={() =>
                                      console.log("Assign To clicked")
                                    }
                                  >
                                    Assign To
                                  </Button>

                                  <Button
                                    color="danger"
                                    size="sm"
                                    onClick={() =>
                                      console.log(
                                        "Add Case to E-Filing clicked"
                                      )
                                    }
                                  >
                                    Add to E-Filing
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
