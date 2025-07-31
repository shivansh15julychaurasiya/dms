import React, { useState } from "react";
import {
  Container,
  Accordion,
  AccordionBody,
  AccordionHeader,
  AccordionItem,
  Row,
  Table,
  Col,
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
import PDFViewerHightLight from "./PDFViewerHightLight";
import PDFViewer from "./PDFViewer"
// Configure PDF worker
pdfjs.GlobalWorkerOptions.workerSrc = `//cdnjs.cloudflare.com/ajax/libs/pdf.js/${pdfjs.version}/pdf.worker.min.js`;

// Static sidebar items

// const sidebarItems = [
//   "Dates and Events [P-3]",
//   "Memo of Civil Revision [P-6]",
//   "Certified copy of Judgment [P-15]",
//   "Formal order/decree [P-24]",
//   "Exemption App [P-28]",
//   "Affidavit [P-30]",
//   "Annexure 1 [P-43]",
//   "Annexure 2 [P-63]",
//   "Annexure 3 [P-74]",
//   "Annexure 4 [P-89]",
//   "Annexure 5 [P-108]",
//   "Annexure 6 [P-114]",
//   "Vakalatnama [P-125]",
//   "Ecourt Fee [P-126]",
//   "Form for Fresh Filing [P-128]",
// ];


const sampleCases = [
  { caseType: "Civil" },
  { caseType: "Criminal" },
  { caseType: "Family" },
];

// Sample document metadata
const documents = [
  {
    id: "1",
    docName: "Office Report",
    court: "Court No. 1",
    bench: "Justice A. Kumar & Justice B. Singh",
    listType: "Daily",
    dol: "29-07-2025",
    fileName: "CauseList_29-07-2025.pdf",
  },
  {
    id: "2",
    docName: "Pettion",
    court: "Court No. 2",
    bench: "Justice X. Yadav",
    listType: "Supplementary",
    dol: "30-07-2025",
    fileName: "CauseList_30-07-2025.pdf",
  },
  {
    id: "3",
    docName: "Pettioners",
    court: "Court No. 2",
    bench: "Justice X. Yadav",
    listType: "Supplementary",
    dol: "30-07-2025",
    fileName: "CauseList_30-07-2025.pdf",
  },
  {
    id: "4",
    docName: "Respondent",
    court: "Court No. 2",
    bench: "Justice X. Yadav",
    listType: "Supplementary",
    dol: "30-07-2025",
    fileName: "CauseList_30-07-2025.pdf",
  },
];

const CaseFileViewer = ({ pdfFileName }) => {
  const [openId, setOpenId] = useState("");
  const highlightPluginInstance = highlightPlugin();
  const filename = pdfFileName || "BAIL10012020_PETN_1";
  const pdfUrl = `http://localhost:8081/dms/api/casesfiles/documents/view/${filename}`;
  const [highlights, setHighlights] = useState([]);

  const toggle = (id) => {
    setOpenId(openId === id ? "" : id);
  };

  return (
    <div className="d-flex">
      <Sidebar/>
      <div className="flex-grow-1">
      <CustomNavbar />

      </div>
      <Container fluid className="p-3 mt-5">
        <Row>
          {/* Sidebar */}
          
          {/* index sidebaarItems heading comment by dev.. */}          
          {/* <Col
            md="2"
            className="bg-light border-end"
            style={{ height: "100vh", overflowY: "auto" }}
          >
            <h6 className="mt-2 text-muted">Index [P-1]</h6>
            <ul className="list-unstyled">
              {sidebarItems.map((item, idx) => (
                <li key={idx} className="mb-1 text-dark small">
                  {item}
                </li>
              ))}
            </ul>
          </Col> */}

          {/* Accordion Section */}
          <Col md="3 mt-4">
            <div className="bg-dark text-light p-2 rounded text-center card">
              <strong>Document Sections</strong>
            </div>
            <div className="mt-4">
              <Accordion open={openId} toggle={toggle}>
                {documents.map((doc) => (
                  <AccordionItem key={doc.id}>
                    <AccordionHeader targetId={`doc-${doc.id}`}>
                      {doc.docName}
                    </AccordionHeader>
                    <AccordionBody accordionId={`doc-${doc.id}`}>
                      <Table  responsive hover>
                        <thead>
                          <tr>
                            <th>Sr No.</th>
                            <th>Case Type</th>
                          </tr>
                        </thead>
                        <tbody>
                          {sampleCases && sampleCases.length > 0 ? (
                            sampleCases.map((item, index) => (
                              <tr key={index}>
                                <td>{index + 1}</td>
                                <td>{item.caseType}</td>
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
                ))}
              </Accordion>
            </div>
          </Col>

          {/* PDF Viewer */}
          <Col md="7" className="p-1">
            <div className="d-flex justify-content-end text-muted small mb-2">
              {new Date().toLocaleString()}
            </div>
            <div className="">

              <PDFViewer/>
            </div>
          </Col>
        </Row>
      </Container>
    </div>
  );
};

export default CaseFileViewer;
