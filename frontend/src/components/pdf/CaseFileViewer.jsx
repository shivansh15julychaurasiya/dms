import React, { useState, useEffect } from "react";
import {
  Container,
  Accordion,
  AccordionBody,
  AccordionHeader,
  AccordionItem,
  Row,
  Table,
  Col,
  Button,
} from "reactstrap";
import { pdfjs } from "react-pdf";
import CustomNavbar from "../layout/Navbar";
import "react-pdf/dist/esm/Page/AnnotationLayer.css";
import "react-pdf/dist/esm/Page/TextLayer.css";
import { Viewer, Worker } from "@react-pdf-viewer/core";
import { highlightPlugin } from "@react-pdf-viewer/highlight";
import "@react-pdf-viewer/core/lib/styles/index.css";
import "@react-pdf-viewer/highlight/lib/styles/index.css";
import Sidebar from "../layout/Sidebar";
import { defaultLayoutPlugin } from "@react-pdf-viewer/default-layout";
import { bookmarkPlugin } from "@react-pdf-viewer/bookmark";
import { useSearchParams } from "react-router-dom";
import { toolbarPlugin } from "@react-pdf-viewer/toolbar";
import "@react-pdf-viewer/toolbar/lib/styles/index.css";

import {
  fetchPdfFileById,
  fetchPdfDetailsFileById,
  fetchOrderReportByFdId,
} from "../../services/PdfFileService";
import { useAuth } from "../../context/AuthContext";

pdfjs.GlobalWorkerOptions.workerSrc = `//cdnjs.cloudflare.com/ajax/libs/pdf.js/${pdfjs.version}/pdf.worker.min.js`;

pdfjs.GlobalWorkerOptions.workerSrc = `//cdnjs.cloudflare.com/ajax/libs/pdf.js/${pdfjs.version}/pdf.worker.min.js`;

const CaseFileViewer = () => {
  const toolbarPluginInstance = toolbarPlugin();
  const { Toolbar } = toolbarPluginInstance;

  const { token } = useAuth();
  const [caseFileDetails, setCaseFileDetails] = useState(null);
  const [pdfUrl, setPdfUrl] = useState(null); // PDF blob URL
  const [orderReport, setOrderReport] = useState([]);

  const [openItems, setOpenItems] = useState([]);
  const [visiblePanel, setVisiblePanel] = useState("bookmark");

  const [searchParams] = useSearchParams();
  const fdId = searchParams.get("id");

  const highlightPluginInstance = highlightPlugin();
  const bookmarkPluginInstance = bookmarkPlugin();
  const defaultLayoutPluginInstance = defaultLayoutPlugin();
  const { Bookmarks } = bookmarkPluginInstance;
  // const Bookmarks = bookmarkPluginInstance.Bookmarks;

  useEffect(() => {
    const fetchData = async () => {
      try {
        const pdfResponse = await fetchPdfFileById(fdId, token);
        const blob = new Blob([pdfResponse.data], { type: "application/pdf" });
        const url = URL.createObjectURL(blob);
        setPdfUrl(url);

        const detailsResponse = await fetchPdfDetailsFileById(fdId, token);
        const textData = await detailsResponse.data.text();
        const jsonData = JSON.parse(textData);

        setCaseFileDetails(jsonData);

        const reportData = await fetchOrderReportByFdId(fdId, token);
        console.log("Order Reports:", reportData);
        setOrderReport(reportData);
      } catch (err) {
        console.error("Error loading PDF and details:", err);
      }
    };

    fetchData();

    return () => {
      // Cleanup object URL
      if (pdfUrl) {
        URL.revokeObjectURL(pdfUrl);
      }
    };
  }, [fdId, token]);

  const toggle = (id) => {
    setOpenItems((prev) =>
      prev.includes(id) ? prev.filter((item) => item !== id) : [...prev, id]
    );
  };

  return (
    <div className="d-flex">
      <div className="flex-grow-1">
        <CustomNavbar />
      </div>
      <Container fluid className="p-3 mt-5">
        <Row>
          {/* Sidebar */}
          <Col
            md="2"
            className="bg-light border-end"
            style={{ height: "100vh", overflowY: "auto" }}
          >
            {visiblePanel === "sidebar" && <Sidebar />}
            {visiblePanel === "bookmark" && (
              <div className="bg-dark text-light p-2">
                <h5 className="mb-2 text-center">Index</h5>
                <div className="bookmark-wrapper">
                  <Bookmarks />
                </div>
              </div>
            )}
          </Col>

          {/* Case details */}
          <Col md="3 mt-4">
            <Row className="mb-3 align-items-center">
              <Col xs="auto">
                <h4 className="dark px-2">{caseFileDetails?.caseType.label} No.{caseFileDetails.fdCaseNo} Of {caseFileDetails.fdCaseYear}</h4>
              </Col>
              <Col xs="auto"></Col>
            </Row>
            <Row className="mb-3 align-items-center">
              <Col xs="auto">
                <Button color="primary" size="sm">
                  View All Orders
                </Button>
              </Col>
              <Col xs="auto">
                <Button color="success" size="sm">
                  Download File
                </Button>
              </Col>
            </Row>
            <div className="mt-4">
              <Accordion
                open={openItems}
                toggle={toggle}
                className="custom-accordion"
              >
                {/* Office Report */}
                <AccordionItem>
                  <AccordionHeader targetId="0">
                    Order/Office Report
                  </AccordionHeader>
                  <AccordionBody accordionId="0">
                    <Table responsive hover>
                      <thead>
                        <tr>
                          <th>Sr No.</th>
                          <th>Type</th>
                          <th>Date</th>
                        </tr>
                      </thead>
                      <tbody>
                        {orderReport?.length > 0 ? (
                          orderReport.map((data, index) => (
                            <tr key={index}>
                              <td>{index + 1}</td>
                              <td>
                                {data.ord_remark}{" "}
                                {data.ord_created
                                  ? new Date(
                                      data.ord_created
                                    ).toLocaleDateString("en-GB")
                                  : ""}
                              </td>
                              <td></td>
                            </tr>
                          ))
                        ) : (
                          <tr>
                            <td colSpan="3" className="text-center">
                              No data available
                            </td>
                          </tr>
                        )}
                      </tbody>
                    </Table>
                  </AccordionBody>
                </AccordionItem>

                {/* Petitioners */}
                <AccordionItem>
                  <AccordionHeader targetId="1" color="black">
                    Petitioners
                  </AccordionHeader>
                  <AccordionBody accordionId="1">
                    <Table responsive hover>
                      <thead>
                        <tr>
                          <th>Sr No.</th>
                          <th>Petitioner Name</th>
                        </tr>
                      </thead>
                      <tbody>
                        {caseFileDetails?.petitioners?.length > 0 ? (
                          caseFileDetails.petitioners.map((item, index) => (
                            <tr key={index}>
                              <td>{index + 1}</td>
                              <td>{item.pt_name}</td>
                            </tr>
                          ))
                        ) : (
                          <tr>
                            <td colSpan="2" className="text-center">
                              No data available
                            </td>
                          </tr>
                        )}
                      </tbody>
                    </Table>
                  </AccordionBody>
                </AccordionItem>

                {/* Respondents */}
                <AccordionItem>
                  <AccordionHeader targetId="2">Respondents</AccordionHeader>
                  <AccordionBody accordionId="2">
                    <Table responsive hover>
                      <thead>
                        <tr>
                          <th>Sr No.</th>
                          <th>Respondent Name</th>
                        </tr>
                      </thead>
                      <tbody>
                        {caseFileDetails?.respondents?.length > 0 ? (
                          caseFileDetails.respondents.map((item, index) => (
                            <tr key={index}>
                              <td>{index + 1}</td>
                              <td>{item.rt_name}</td>
                            </tr>
                          ))
                        ) : (
                          <tr>
                            <td colSpan="2" className="text-center">
                              No data available
                            </td>
                          </tr>
                        )}
                      </tbody>
                    </Table>
                  </AccordionBody>
                </AccordionItem>
              </Accordion>
            </div>
          </Col>

          {/* PDF Viewer */}
          <Col md="7">
            <Button
              color={visiblePanel === "bookmark" ? "primary" : "secondary"}
              onClick={() =>
                setVisiblePanel((prev) =>
                  prev === "bookmark" ? "sidebar" : "bookmark"
                )
              }
            >
              <i className="bi bi-bookmarks-fill me-2"></i>
            </Button>

            <div className="mt-3">
              <Worker workerUrl="https://unpkg.com/pdfjs-dist@3.11.174/build/pdf.worker.min.js">
                <div
                  style={{
                    height: "100vh",
                    display: "flex",
                    flexDirection: "column",
                  }}
                >
                  {/* Toolbar */}
                  <div
                    style={{ borderBottom: "1px solid #ccc", padding: "4px" }}
                  >
                    <Toolbar />
                  </div>

                  {/* Main PDF area */}
                  <div style={{ flex: 1, overflow: "auto" }}>
                    {pdfUrl ? (
                      <Viewer
                        fileUrl={pdfUrl}
                        plugins={[
                          toolbarPluginInstance,
                          highlightPluginInstance,
                          bookmarkPluginInstance, // pass here
                        ]}
                      />
                    ) : (
                      <p>Loading PDF...</p>
                    )}
                  </div>
                </div>
              </Worker>
            </div>
          </Col>
        </Row>
      </Container>
    </div>
  );
};

export default CaseFileViewer;
