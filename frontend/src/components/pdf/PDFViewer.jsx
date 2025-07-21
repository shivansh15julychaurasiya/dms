// src/components/pdf/PdfViewer.js
import { Worker, Viewer } from '@react-pdf-viewer/core';
import { defaultLayoutPlugin } from '@react-pdf-viewer/default-layout';
import '@react-pdf-viewer/core/lib/styles/index.css';
import '@react-pdf-viewer/default-layout/lib/styles/index.css';
import { useParams } from 'react-router-dom';

const PdfViewer = () => {
  const { fileName } = useParams();
  const defaultLayoutPluginInstance = defaultLayoutPlugin();

  const pdfUrl = `http://localhost:8081/dms/api/casesfiles/documents/view/${fileName}`;

  return (
    <div style={{ height: '100vh' }}>
      <Worker workerUrl={`https://unpkg.com/pdfjs-dist@3.11.174/build/pdf.worker.min.js`}>
        <Viewer
          fileUrl={pdfUrl}
          plugins={[defaultLayoutPluginInstance]}
        />
      </Worker>
    </div>
  );
};

export default PdfViewer;
