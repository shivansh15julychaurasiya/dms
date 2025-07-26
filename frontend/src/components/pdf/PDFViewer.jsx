import { useState, useEffect } from "react";
import {
  Button,
  Badge,
  Row,
  Col,
  ListGroup,
  ListGroupItem,
} from "reactstrap";
import {
  Viewer,
  Worker,
} from "@react-pdf-viewer/core";
import { highlightPlugin } from "@react-pdf-viewer/highlight";
import "@react-pdf-viewer/core/lib/styles/index.css";
import "@react-pdf-viewer/highlight/lib/styles/index.css";

function PdfViewer({ pdfFileName }) {
  const highlightPluginInstance = highlightPlugin();
  const { jumpToHighlightArea } = highlightPluginInstance;

  const filename = pdfFileName || "LeaveApp";
  const pdfUrl = `http://localhost:8081/dms/api/casesfiles/documents/view/${filename}`;

  const [highlights, setHighlights] = useState([]);

  const indexItems = [
    { label: "Index [P-1]", highlightId: "p1" },
    { label: "Dates and Events [P-3]", highlightId: "p3" },
    { label: "Memo of Civil Revision [P-6]", highlightId: "p6" },
    { label: "Certified copy [P-15]", highlightId: "p15" },
    { label: "Annexure 1 [P-43]", highlightId: "p43" },
    { label: "Annexure 2 [P-63]", highlightId: "p63" },
    { label: "Affidavit [P-30]", highlightId: "p30" },
  ];

  // Optional: preload highlights
  useEffect(() => {
    const sampleHighlights = indexItems.map((item, i) => ({
      id: item.highlightId,
      pageIndex: i, // You should map these to actual pages
      height: 50,
      top: 100,
      left: 100,
      width: 200,
      content: item.label,
    }));
    setHighlights(sampleHighlights);
  }, []);

  return (
    <Row style={{ height: "100vh" }}>
      {/* Left Sidebar (Index) */}
      <Col md="2" className="border-end bg-light p-2" style={{ overflowY: "auto" }}>
        <h6 className="mb-3 text-secondary">Index</h6>
        <ListGroup flush>
          {indexItems.map((item, idx) => (
            <ListGroupItem
              key={idx}
              tag="button"
              action
              onClick={() => jumpToHighlightArea(item.highlightId)}
              className="py-2 small text-start"
            >
              {item.label}
            </ListGroupItem>
          ))}
        </ListGroup>
      </Col>

      {/* Center Buttons */}
      <Col md="2" className="border-end bg-white p-3 text-center">
        <h6 className="text-secondary mb-3">Actions</h6>
        <Button color="success" size="sm" className="mb-2 w-100">View All Orders</Button>
        <Button color="primary" size="sm" className="mb-2 w-100">Download File</Button>
        <Button color="danger" size="sm" className="w-100">
          Objection <Badge color="light">1</Badge>
        </Button>
      </Col>

      {/* Right PDF Viewer */}
      <Col md="8" className="p-3">
        <div className="d-flex justify-content-end text-muted small mb-2">
          {new Date().toLocaleString()}
        </div>
        <div style={{ height: "90vh", border: "1px solid #ccc" }}>
          <Worker workerUrl="https://unpkg.com/pdfjs-dist@3.11.174/build/pdf.worker.min.js">
            <Viewer
              fileUrl={pdfUrl}
              plugins={[highlightPluginInstance]}
              renderHighlights={(props) =>
                highlights.map((h) => (
                  <div
                    key={h.id}
                    data-annotation-id={h.id}
                    style={{
                      background: "rgba(255,255,0,0.4)",
                      position: "absolute",
                      left: `${h.left}px`,
                      top: `${h.top}px`,
                      height: `${h.height}px`,
                      width: `${h.width}px`,
                      cursor: "pointer",
                    }}
                    onClick={() => alert(`Clicked on ${h.content}`)}
                  />
                ))
              }
            />
          </Worker>
        </div>
      </Col>
    </Row>
  );
}

export default PdfViewer;
