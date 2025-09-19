import { useEffect, useRef, useState } from "react";
import * as pdfjsLib from "pdfjs-dist/legacy/build/pdf";
import { GlobalWorkerOptions } from "pdfjs-dist/legacy/build/pdf";
import TreeView from "../../components/pdf/TreeView";
import { fetchPdfFileById } from "../../services/PdfFileService";
import { useAuth } from "../../context/AuthContext";
import { useSearchParams } from "react-router-dom";
import CustomNavbar from "../layout/Navbar";
import Sidebar from "../../components/layout/Sidebar";

// Path to your worker from public/pdfjs/build/pdf.worker.js
GlobalWorkerOptions.workerSrc = "/dms/pdfjs/build/pdf.worker.js";

// Recursive Outline Renderer
const OutlineList = ({ outline, onClick }) => {
  if (!outline || outline.length === 0) return null;

  return (
    <ul style={{ listStyle: "none", paddingLeft: "10px" }}>
      {outline.map((item, idx) => (
        <li key={idx}>
          <a
            href="#"
            onClick={(e) => {
              e.preventDefault();
              onClick(item);
            }}
            style={{
              display: "block",
              padding: "2px 0",
              textDecoration: "none",
              color: "#fff",
               fontSize: "12px"
            }}
          >
            {item.title || "Untitled"}
          </a>

          {/* render children recursively */}
          {item.items && item.items.length > 0 && (
            <OutlineList outline={item.items} onClick={onClick} />
          )}
        </li>
      ))}
    </ul>
  );
};

const ShowFileViewer = () => {
  // **************************** FULLSTACK JAVA DEVELOPER VIJAY CHAURASIYA ***********************************

  const { token } = useAuth();
  const [pdfUrl, setPdfUrl] = useState();
  const [activeDoc, setActiveDoc] = useState("pdfUrl");
  const [allOrderPdf, setAllOrderPdf] = useState([]);
  const [searchParams] = useSearchParams();
  const [outlineData, setOutlineData] = useState([]);
  const [visiblePanel, setVisiblePanel] = useState("outline");

  const fdId = searchParams.get("id");
  const iframeRef = useRef(null);
  const pdfRef = useRef(null); // keep reference of loaded pdf

  // Fetch PDF
  useEffect(() => {
    if (fdId && token) {
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

  // Load PDF outline
  useEffect(() => {
console.log("Document pdf url="+pdfUrl)

    if (!pdfUrl || visiblePanel !== "outline") return;

    const loadOutline = async () => {
      try {
        const loadingTask = pdfjsLib.getDocument(pdfUrl);
        const pdf = await loadingTask.promise;
        pdfRef.current = pdf; // save reference
        const outline = await pdf.getOutline();
        setOutlineData(outline || []);
      } catch (err) {
        console.error("Failed to load PDF outline:", err);
        setOutlineData([]);
      }
    };

    loadOutline();
  }, [pdfUrl, visiblePanel]);

  // Handle outline click
  const handleOutlineClick = async (item) => {
    if (!pdfRef.current || !iframeRef.current || !item.dest) return;

    try {
      const dest =
        typeof item.dest === "string"
          ? await pdfRef.current.getDestination(item.dest)
          : item.dest;

      if (dest) {
        const pageIndex = await pdfRef.current.getPageIndex(dest[0]);
        const pageNumber = pageIndex + 1;

        iframeRef.current.contentWindow.postMessage(
          { type: "page", pageNumber },
          "*"
        );
      }
    } catch (err) {
      console.error("Failed to resolve destination:", err);
    }
  };

  // Inject custom toggle button into PDF.js toolbar
  useEffect(() => {
    if (!iframeRef.current) return;

    const handleLoad = () => {
      const iframe = iframeRef.current;
      if (!iframe?.contentWindow) return;

      const script = `
      (function() {
        const toolbar = document.getElementById('toolbarViewerLeft');
        if (!toolbar) return;

        if (!document.getElementById('customToggleBtn')) {
          const btn = document.createElement('button');
          btn.id = 'customToggleBtn';
          btn.className = 'toolbarButton';
          btn.title = 'Toggle Sidebar / Outline';

          const iconSpan = document.createElement('span');
          iconSpan.className = 'toolbarButtonIcon bookmarkIcon';
          btn.appendChild(iconSpan);

          btn.addEventListener('click', function () {
            window.parent.postMessage({ type: 'togglePanel' }, '*');
          });

          toolbar.insertBefore(btn, toolbar.firstChild);
        }
      })();
    `;

      iframe.contentWindow.eval(script);
    };

    iframeRef.current.addEventListener("load", handleLoad);
    return () => {
      iframeRef.current?.removeEventListener("load", handleLoad);
    };
  }, [pdfUrl]);

  // Listen to toggle message
  useEffect(() => {
    const handleMessage = (event) => {
      if (event.data?.type === "togglePanel") {
        setVisiblePanel((prev) =>
          prev === "sidebar" ? "outline" : "sidebar"
        );
      }
    };
    window.addEventListener("message", handleMessage);
    return () => window.removeEventListener("message", handleMessage);
  }, []);

  return (
    <div className="d-flex flex-column vh-100  ">
      <CustomNavbar />
      <div className="container-fluid flex-grow-1 ">
        <div className="row h-100 pt-5">
         {/* col-2 */}
<div className="col-2 border-end overflow-auto p-2 bg-dark ">
  {visiblePanel === "outline" ? (
    outlineData.length > 0 ? (
      <div id="outlineView">
        <OutlineList outline={outlineData} onClick={handleOutlineClick} />
      </div>
    ) : (
      <p style={{ color: "white" }}>No outline found</p>
    )
  ) : (
    <Sidebar  styles={{ width: "225px", backgroundColor: "#222" }} />
  )}
</div>


          {/* col-3 */}
          <div className="col-3 border-end overflow-auto p-2 mt-5">
            <TreeView
              setPdfUrl={setPdfUrl}
              setAllOrderPdf={setAllOrderPdf}
              setActiveDoc={setActiveDoc}
            />
          </div>

          {/* col-7 */}
        <div className="col-7 ">
  <iframe
    ref={iframeRef}
    src={
      pdfUrl
        ? `/dms/pdfjs/web/viewer.html?file=${encodeURIComponent(pdfUrl)}`
        : `/dms/pdfjs/web/viewer.html?file=/pdf/blank.pdf` //use blank fallback
    }
    title="PDF Viewer"
    width="100%"
    height="100%"
    style={{ border: "none" }}
  />
</div>


        </div>
      </div>
    </div>
  );
};

export default ShowFileViewer;
