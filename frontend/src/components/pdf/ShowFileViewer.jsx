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

const ShowFileViewer = () => {
  const { token } = useAuth();
  const [pdfUrl, setPdfUrl] = useState();
  const [activeDoc, setActiveDoc] = useState("pdfUrl");
  const [allOrderPdf, setAllOrderPdf] = useState([]);
  const [searchParams] = useSearchParams();
  const [outlineData, setOutlineData] = useState([]);
  const [visiblePanel, setVisiblePanel] = useState("outline");

  const fdId = searchParams.get("id");
  const iframeRef = useRef(null);

  // Fetch PDF
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

  // Load PDF outline
  useEffect(() => {
    if (!pdfUrl || visiblePanel !== "outline") return;

    const loadOutline = async () => {
      try {
        const loadingTask = pdfjsLib.getDocument(pdfUrl);
        const pdf = await loadingTask.promise;
        const outline = await pdf.getOutline();
        setOutlineData(outline || []);

        const container = document.getElementById("outlineView");
        if (!container) return;
        container.innerHTML = "";

        if (outline && outline.length > 0) {
          outline.forEach((item) => {
            const a = document.createElement("a");
            a.href = "#";
            a.textContent = item.title || "Untitled";
            a.style.display = "block";
            a.style.padding = "2px 0";
            a.style.textDecoration = "none";
            a.style.color = "#fff";
            a.onmouseover = () => (a.style.background = "#333");
            a.onmouseout = () => (a.style.background = "transparent");

            a.onclick = async (e) => {
              e.preventDefault();
              if (iframeRef.current && item.dest) {
                try {
                  const dest =
                    typeof item.dest === "string"
                      ? await pdf.getDestination(item.dest)
                      : item.dest;

                  if (dest) {
                    const pageIndex = await pdf.getPageIndex(dest[0]);
                    const pageNumber = pageIndex + 1;

                    iframeRef.current.contentWindow.postMessage(
                      { type: "page", pageNumber },
                      "*"
                    );
                  }
                } catch (err) {
                  console.error("Failed to resolve destination:", err);
                }
              }
            };

            container.appendChild(a);
          });
        } else {
          container.innerHTML = "<p style='color:white'>No outline found</p>";
        }
      } catch (err) {
        console.error("Failed to load PDF outline:", err);
      }
    };

    loadOutline();
  }, [pdfUrl, visiblePanel]);

  // Inject custom toggle button into PDF.js toolbar
  useEffect(() => {
    if (!iframeRef.current) return;

    const handleLoad = () => {
      const iframe = iframeRef.current;
      if (!iframe?.contentWindow) return;

      // Inject script inside PDF.js iframe
      const script = `
      (function() {
       
        // 2 Add custom toggle button if not exists
        const toolbar = document.getElementById('toolbarViewerLeft');
        if (!toolbar) return;

        if (!document.getElementById('customToggleBtn')) {
          const btn = document.createElement('button');
          btn.id = 'customToggleBtn';
          btn.className = 'toolbarButton';
          btn.title = 'Toggle Sidebar / Outline';

          // Use PDF.js built-in bookmark icon
          const iconSpan = document.createElement('span');
          iconSpan.className = 'toolbarButtonIcon bookmarkIcon';
          btn.appendChild(iconSpan);

          btn.addEventListener('click', function () {
            window.parent.postMessage({ type: 'togglePanel' }, '*');
          });

          // Add button to the left side of toolbar
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
        setVisiblePanel((prev) => (prev === "sidebar" ? "outline" : "sidebar"));
      }
    };
    window.addEventListener("message", handleMessage);
    return () => window.removeEventListener("message", handleMessage);
  }, []);

  return (
    <div className="d-flex flex-column vh-100">
      <CustomNavbar />
      <div className="container-fluid flex-grow-1 mt-1">
        <div className="row h-100">
          {/* col-2 */}
          <div className="col-2 border-end overflow-auto p-2 bg-dark">
            {visiblePanel === "outline" && outlineData.length > 0 ? (
              <div id="outlineView"></div>
            ) : (
              <Sidebar />
            )}
          </div>

          {/* col-3 */}
          <div className="col-3 border-end overflow-auto p-2">
            <TreeView
              setPdfUrl={setPdfUrl}
              setAllOrderPdf={setAllOrderPdf}
              setActiveDoc={setActiveDoc}
            />
          </div>

          {/* col-7 */}
          <div className="col-7 p-0">
            {pdfUrl && (
              <iframe
                ref={iframeRef}
                src={`/dms/pdfjs/web/viewer.html?file=${encodeURIComponent(
                  pdfUrl
                )}`}
                title="PDF Viewer"
                width="100%"
                height="100%"
                style={{ border: "none" }}
              />
            )}
          </div>
        </div>
      </div>
    </div>
  );
};

export default ShowFileViewer;
