import React, { useState, useEffect } from "react";
import { Container, Row, Col, Button } from "reactstrap";
import CustomNavbar from "../layout/Navbar";
import { highlightPlugin, RenderHighlightTargetProps } from '@react-pdf-viewer/highlight';
import { Position, Tooltip } from '@react-pdf-viewer/core';

import { Viewer, Worker } from "@react-pdf-viewer/core";
import "@react-pdf-viewer/core/lib/styles/index.css";
import "@react-pdf-viewer/highlight/lib/styles/index.css";
import { ProgressBar } from "@react-pdf-viewer/core";
import Sidebar from "../layout/Sidebar";
import { bookmarkPlugin } from "@react-pdf-viewer/bookmark";
import { useSearchParams } from "react-router-dom";
import { toolbarPlugin } from "@react-pdf-viewer/toolbar";
import "@react-pdf-viewer/toolbar/lib/styles/index.css";
import { MinimalButton } from "@react-pdf-viewer/core";
import { MessageCircle as MessageIcon } from "lucide-react"; // Example icon

import "@react-pdf-viewer/core/lib/styles/index.css";
import "@react-pdf-viewer/highlight/lib/styles/index.css";
import TreeView from "./TreeView";
import { fetchPdfFileById } from "../../services/PdfFileService";
import { useAuth } from "../../context/AuthContext";

const CaseFileViewer = () => {
  const toolbarPluginInstance = toolbarPlugin();
  const { Toolbar } = toolbarPluginInstance;

  const { token } = useAuth();
  const [pdfUrl, setPdfUrl] = useState();
  const [allOrderPdf, setAllOrderPdf] = useState([]);
  const [hasBookmarks, setHasBookmarks] = useState(true);
  const [visiblePanel, setVisiblePanel] = useState("bookmark");
  const [searchParams] = useSearchParams();
  const fdId = searchParams.get("id");
  const [activeDoc, setActiveDoc] = useState("pdfUrl");
  const [message, setMessage] = React.useState('');
  // "pdfUrl" or "allOrderPdf"

  const [notes,setNotes]=useState([]);

  // Notes state with localStorage init
// const [notes, setNotes] = useState(() => {
//     const saved = localStorage.getItem("pdfHighlights");
//     return saved ? JSON.parse(saved) : [];
// });

// Persist notes on change
// useEffect(() => {
//     localStorage.setItem("pdfHighlights", JSON.stringify(notes));
// }, [notes]);

useEffect(() => {
    if (pdfUrl) {
        setNotes([]);   // Clear highlights when new PDF is loaded
    }
}, [pdfUrl]);


const renderHighlightTarget = (props) => (
        <div
            style={{
                background: "#eeeeee", // fixed color
                display: "flex",
                position: "absolute",
                left: `${props.selectionRegion.left}%`,
                top: `${props.selectionRegion.top + props.selectionRegion.height}%`,
                transform: "translate(0, 8px)",
                zIndex: 10,
            }}
        >
            <Tooltip
                position={Position.TopCenter}
                target={
                    <Button onClick={props.toggle}>
                        <MessageIcon size={16} />
                    </Button>
                }
                content={() => <div style={{ width: "100px" }}>Add a note</div>}
                offset={{ left: 0, top: -8 }}
            />
        </div>
    );

 


//     const renderHighlightContent = (props) => {
//     const addNote = () => {
//         // We will implement it later
//         console.log(message);
//     };

//     return (
//         <div
//             style={{
//                 background: '#fff',
//                 border: '1px solid rgba(0, 0, 0, .3)',
//                 borderRadius: '2px',
//                 padding: '8px',
//                 position: 'absolute',
//                 left: `${props.selectionRegion.left}%`,
//                 top: `${props.selectionRegion.top + props.selectionRegion.height}%`,
//                 zIndex: 1,
//             }}
//         >
//             <div>
//                 <textarea
//                     rows={3}
//                     style={{
//                         border: '1px solid rgba(0, 0, 0, .3)',
//                     }}
//                     onChange={(e) => setMessage(e.target.value)}
//                 ></textarea>
//             </div>
//             <div
//                 style={{
//                     display: 'flex',
//                     marginTop: '8px',
//                 }}
//             >
//                 <div style={{ marginRight: '8px' }}>
//                     <Button color="primary" onClick={addNote}>Add</Button>

//                 </div>
//                 <Button onClick={props.cancel}>Cancel</Button>
//             </div>
//         </div>
//     );
// };
useEffect(() => {
    console.log("Current notes:", notes);
}, [notes]);
const renderHighlightContent = (props) => {
        const saveNote = () => {
            setNotes([
                ...notes,
                {
                    highlightAreas: props.highlightAreas,
                    comment: props.selectedText,
                },
            ]);
            props.cancel(); 
        };

        return (
            <div
                style={{
                    background: "white",
                    border: "1px solid #aaa",
                    padding: "8px",
                    position: "absolute",
                    left: `${props.selectionRegion.left}%`,
                    top: `${props.selectionRegion.top + props.selectionRegion.height}%`,
                    zIndex: 20,
                }}
            >
                <button onClick={saveNote}>Save</button>
                <button onClick={props.cancel}>Cancel</button>
            </div>
        );
    };

 const renderHighlights = (props) => (
        <>
            {notes.map((note, idx) => (
                <React.Fragment key={idx}>
                    {note.highlightAreas
                        .filter((area) => area.pageIndex === props.pageIndex)
                        .map((area, j) => (
                            <div
                                key={j}
                                style={{
                                    background: "rgba(255, 255, 0, 0.4)",
                                    left: `${area.left}%`,
                                    top: `${area.top}%`,
                                    height: `${area.height}%`,
                                    width: `${area.width}%`,
                                    position: "absolute",
                                }}
                            />
                        ))}
                </React.Fragment>
            ))}
        </>
    );

   const highlightPluginInstance = highlightPlugin({
        renderHighlightTarget,
        renderHighlightContent,
        renderHighlights
    });




  const bookmarkPluginInstance = bookmarkPlugin();
  const { Bookmarks } = bookmarkPluginInstance;

  useEffect(() => {
    if (fdId && token && !pdfUrl) {
      const fetchData = async () => {
        try {
          const pdfResponse = await fetchPdfFileById(fdId, token);
          const blob = new Blob([pdfResponse.data], {
            type: "application/pdf",
          });
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
                  {hasBookmarks ? (
                    <Bookmarks />
                  ) : (
                    <Sidebar
                      styles={{ width: "225px", backgroundColor: "#222" }}
                    />
                  )}
                </div>
              </div>
            )}
          </Col>

          {/* Case details */}
          <Col md="3 mt-4">
            <TreeView
              setPdfUrl={setPdfUrl}
              setAllOrderPdf={setAllOrderPdf}
              setActiveDoc={setActiveDoc}
            />
          </Col>

          {/* PDF Viewer */}
          <Col md="7">
            <div className="mt-3">
              {activeDoc === "allOrderPdf" && allOrderPdf.length > 0 ? (
                // Show allOrderPdf viewer
                <Worker workerUrl="https://unpkg.com/pdfjs-dist@3.11.174/build/pdf.worker.min.js">
                  <div style={{ height: "100vh", overflow: "auto" }}>
                    {allOrderPdf.map((url, index) => (
                      <div
                        key={index}
                        style={{
                          height: "90vh",
                          marginBottom: "30px",
                          border: "1px solid #ccc",
                        }}
                      >
                        <div
                          style={{
                            alignItems: "center",
                            display: "flex",
                          }}
                        >
                          <div style={{ marginRight: "0.25rem" }}>
                            <MinimalButton
                              color={
                                visiblePanel === "bookmark"
                                  ? "primary"
                                  : "secondary"
                              }
                              onClick={() =>
                                setVisiblePanel((prev) =>
                                  prev === "bookmark" ? "sidebar" : "bookmark"
                                )
                              }
                            ></MinimalButton>
                          </div>
                          <Toolbar />
                        </div>

                        <Viewer
                        theme='dark'
                          key={pdfUrl}
                          fileUrl={pdfUrl}
                          plugins={[
                            toolbarPluginInstance,
                            highlightPluginInstance,
                            bookmarkPluginInstance,
                          ]}
                          onDocumentLoad={async ({ doc }) => {
                            const outline = await doc.getOutline();
                            setHasBookmarks(outline && outline.length > 0);
                          }}
                          renderLoader={(percentages) => (
                            <div style={{ width: "300px" }}>
                              <ProgressBar
                                now={Math.round(percentages)}
                                label={`${Math.round(percentages)}%`}
                              />
                            </div>
                          )}
                        />
                      </div>
                    ))}
                  </div>
                </Worker>
              ) : activeDoc === "pdfUrl" && pdfUrl ? (
                // Show single pdfUrl viewer
                <Worker workerUrl="https://unpkg.com/pdfjs-dist@3.11.174/build/pdf.worker.min.js">
                  <div
                    style={{
                      height: "100vh",
                      display: "flex",
                      flexDirection: "column",
                    }}
                  >
                    <div
                      style={{
                        display: "flex",
                        alignItems: "center",
                        borderBottom: "1px solid #ccc",
                        padding: "4px",
                      }}
                    >
                      <MinimalButton
                        color={
                          visiblePanel === "bookmark" ? "primary" : "secondary"
                        }
                        onClick={() =>
                          setVisiblePanel((prev) =>
                            prev === "bookmark" ? "sidebar" : "bookmark"
                          )
                        }
                      >
                        <i
                          className="bi bi-bookmarks-fill"
                          style={{ fontSize: "20px" }}
                        />
                      </MinimalButton>
                      <Toolbar />
                    </div>

                    <div style={{ flex: 1, overflow: "auto" }}>
                      <Viewer
                      theme='dark'
                        key={pdfUrl}
                        fileUrl={pdfUrl}
                        plugins={[
                          toolbarPluginInstance,
                          highlightPluginInstance,
                          bookmarkPluginInstance,
                        ]}
                        onDocumentLoad={async ({ doc }) => {
                          const outline = await doc.getOutline();
                          setHasBookmarks(outline && outline.length > 0);
                        }}
                        renderLoader={(percentages) => (
                          <div style={{ width: "240px" }}>
                            <ProgressBar
                              now={Math.round(percentages)}
                              label={`${Math.round(percentages)}%`}
                            />
                          </div>
                        )}
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
