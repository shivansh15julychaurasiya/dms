import React, { useState, useEffect } from "react";
import { Container, Row, Col, Button } from "reactstrap";
import { pdfjs } from "react-pdf";
import CustomNavbar from "../layout/Navbar";
import "react-pdf/dist/esm/Page/AnnotationLayer.css";
import "react-pdf/dist/esm/Page/TextLayer.css";
import { Viewer, Worker } from "@react-pdf-viewer/core";
import { highlightPlugin } from "@react-pdf-viewer/highlight";
import "@react-pdf-viewer/core/lib/styles/index.css";
import "@react-pdf-viewer/highlight/lib/styles/index.css";
import Sidebar from "../layout/Sidebar";
import { bookmarkPlugin } from "@react-pdf-viewer/bookmark";
import { useSearchParams } from "react-router-dom";
import { toolbarPlugin } from "@react-pdf-viewer/toolbar";
import "@react-pdf-viewer/toolbar/lib/styles/index.css";
import TreeView from "./TreeView";
import { fetchPdfFileById } from "../../services/PdfFileService";
import { useAuth } from "../../context/AuthContext";

pdfjs.GlobalWorkerOptions.workerSrc = `//cdnjs.cloudflare.com/ajax/libs/pdf.js/${pdfjs.version}/pdf.worker.min.js`;

const CaseFileViewer = () => {
  const toolbarPluginInstance = toolbarPlugin();
  const { Toolbar } = toolbarPluginInstance;

  const { token } = useAuth();
  const [pdfUrl, setPdfUrl] = useState(null);
  const [hasBookmarks, setHasBookmarks] = useState(true); // 👈 new state

  const [visiblePanel, setVisiblePanel] = useState("bookmark");
  const [searchParams] = useSearchParams();
  const fdId = searchParams.get("id");

  const highlightPluginInstance = highlightPlugin();
  const bookmarkPluginInstance = bookmarkPlugin();
  const { Bookmarks } = bookmarkPluginInstance;

  useEffect(() => {
    if (fdId && token && !pdfUrl) {
      const fetchData = async () => {
        try {
          const pdfResponse = await fetchPdfFileById(fdId, token);
          const blob = new Blob([pdfResponse.data], { type: "application/pdf" });
          const url = URL.createObjectURL(blob);
          setPdfUrl(url);
        } catch (err) {
          console.error("Error loading PDF:", err);
        }
      };
      fetchData();
    }

    return () => {
      if (pdfUrl) URL.revokeObjectURL(pdfUrl);
    };
  }, [fdId, token]);

  return (
    <div className="d-flex">
      <div className="flex-grow-1">
        <CustomNavbar />
      </div>
      <Container fluid className="p-1 mt-5">
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
                 {hasBookmarks ? <Bookmarks /> : <Sidebar styles={{ width: "225px", backgroundColor: "#222" }} />}
                </div>
              </div>
            )}
          </Col>

          {/* Case details */}
          <Col md="3 mt-4">
            <TreeView setPdfUrl={setPdfUrl} />
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
              {pdfUrl ? (
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
                      <Viewer
                        key={pdfUrl}
                        fileUrl={pdfUrl}
                        plugins={[
                          toolbarPluginInstance,
                          highlightPluginInstance,
                          bookmarkPluginInstance,
                        ]}
                        onDocumentLoad={async (e) => {
                          const outline = await e.doc.getOutline();
                          setHasBookmarks(outline && outline.length > 0);
                        }}
                      />
                    </div>
                  </div>
                </Worker>
              ) : (
                <p style={{ padding: "20px", textAlign: "center" }}>
                  Please select a file to view
                </p>
              )}
            </div>
          </Col>
        </Row>
      </Container>
    </div>
  );
};

export default CaseFileViewer;
