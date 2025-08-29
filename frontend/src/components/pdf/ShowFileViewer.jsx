import React, { useEffect, useState } from "react";
import { Viewer, Worker } from "@react-pdf-viewer/core";
import { highlightPlugin } from "@react-pdf-viewer/highlight";
import { defaultLayoutPlugin } from "@react-pdf-viewer/default-layout";
import { bookmarkPlugin } from "@react-pdf-viewer/bookmark";
import { toolbarPlugin } from "@react-pdf-viewer/toolbar";
import { pdfjs } from "react-pdf";

import "@react-pdf-viewer/core/lib/styles/index.css";
import "@react-pdf-viewer/highlight/lib/styles/index.css";
import "@react-pdf-viewer/toolbar/lib/styles/index.css";
pdfjs.GlobalWorkerOptions.workerSrc = `//cdnjs.cloudflare.com/ajax/libs/pdf.js/${pdfjs.version}/pdf.worker.min.js`;

pdfjs.GlobalWorkerOptions.workerSrc = `//cdnjs.cloudflare.com/ajax/libs/pdf.js/${pdfjs.version}/pdf.worker.min.js`;

import { fetchPdfFileById } from "../../services/PdfFileService";

const ShowFileViewer = ({ pdfUrl}) => {
//   const [pdfUrl, setPdfUrl] = useState(null);
console.log("pdf url="+pdfUrl)
  const toolbarPluginInstance = toolbarPlugin();
  const { Toolbar } = toolbarPluginInstance;
  const highlightPluginInstance = highlightPlugin();
  const bookmarkPluginInstance = bookmarkPlugin();
  const defaultLayoutPluginInstance = defaultLayoutPlugin();


  return (
    <div style={{ height: "100vh", display: "flex", flexDirection: "column" }}>
      {/* Toolbar */}
      <div style={{ borderBottom: "1px solid #ccc", padding: "4px" }}>
        <Toolbar />
      </div>

      {/* Main PDF area */}
      <div style={{ flex: 1, overflow: "auto" }}>
        {pdfUrl ? (
          <Worker workerUrl="https://unpkg.com/pdfjs-dist@3.11.174/build/pdf.worker.min.js">
            <Viewer
              fileUrl={pdfUrl}
              plugins={[
                toolbarPluginInstance,
                highlightPluginInstance,
                bookmarkPluginInstance,
                defaultLayoutPluginInstance,
              ]}
            />
          </Worker>
        ) : (
          <p>Loading PDF...</p>
        )}
      </div>
    </div>
  );
};

export default ShowFileViewer;
