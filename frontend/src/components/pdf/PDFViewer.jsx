// PDFViewer.js
import React from "react";

// Core viewer
import { Viewer } from "@react-pdf-viewer/core";

// Plugins
import { defaultLayoutPlugin } from "@react-pdf-viewer/default-layout";
import { highlightPlugin } from "@react-pdf-viewer/highlight";
import { searchPlugin } from "@react-pdf-viewer/search";

// Import styles
import "@react-pdf-viewer/core/lib/styles/index.css";
import "@react-pdf-viewer/default-layout/lib/styles/index.css";
import "@react-pdf-viewer/search/lib/styles/index.css";
import "@react-pdf-viewer/highlight/lib/styles/index.css";

const PDFViewer = () => {
  // Plugin instances
  const defaultLayoutPluginInstance = defaultLayoutPlugin();
  const highlightPluginInstance = highlightPlugin();
  const searchPluginInstance = searchPlugin();

  return (
    <div style={{ height: "100vh", width: "100%" }}>
      <Viewer
        fileUrl="/Sample.pdf" // Make sure the file is in public/ folder
        plugins={[
          defaultLayoutPluginInstance,
          highlightPluginInstance,
          searchPluginInstance,
        ]}
      />
    </div>
  );
};

export default PDFViewer;
