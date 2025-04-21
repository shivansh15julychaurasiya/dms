import React, { useEffect, useRef, useState } from "react";
import * as pdfjsLib from "pdfjs-dist";
import "pdfjs-dist/web/pdf_viewer.css";

// Configure PDF.js worker
pdfjsLib.GlobalWorkerOptions.workerSrc = `//cdnjs.cloudflare.com/ajax/libs/pdf.js/${pdfjsLib.version}/pdf.worker.js`;

const PDFHighlighter = ({ pdfUrl }) => {
  const canvasRef = useRef(null);
  const textLayerRef = useRef(null);
  const [pdf, setPdf] = useState(null);
  const [currentPage, setCurrentPage] = useState(1);
  const [totalPages, setTotalPages] = useState(0);
  const [searchText, setSearchText] = useState("");
  const [highlights, setHighlights] = useState({});

  // Load the PDF
  useEffect(() => {
    const loadPDF = async () => {
      if (!pdfUrl) {
        console.warn("pdfUrl is missing or invalid:", pdfUrl);
        return;
      }

      try {
        const loadingTask = pdfjsLib.getDocument(pdfUrl);
        const pdfDoc = await loadingTask.promise;
        setPdf(pdfDoc);
        setTotalPages(pdfDoc.numPages);

        const saved = localStorage.getItem("pdf-highlights");
        if (saved) setHighlights(JSON.parse(saved));

        await renderPage(pdfDoc, currentPage);
      } catch (err) {
        console.error("Error loading PDF:", err);
      }
    };

    const renderPage = async (pdfDoc, pageNumber) => {
      const page = await pdfDoc.getPage(pageNumber);
      const viewport = page.getViewport({ scale: 1.5 });

      const canvas = canvasRef.current;
      const context = canvas.getContext("2d");
      canvas.height = viewport.height;
      canvas.width = viewport.width;

      await page.render({ canvasContext: context, viewport }).promise;

      const textLayerDiv = textLayerRef.current;
      textLayerDiv.innerHTML = "";
      const textContent = await page.getTextContent();

      pdfjsLib.renderTextLayer({
        textContent,
        container: textLayerDiv,
        viewport,
        textDivs: [],
        enhanceTextSelection: true
      });

      // Re-apply highlights
      setTimeout(() => {
        const savedHighlights = highlights[pageNumber] || [];
        savedHighlights.forEach(({ text }) => {
          [...textLayerDiv.childNodes].forEach((child) => {
            const regex = new RegExp(`(${text})`, "gi");
            child.innerHTML = child.textContent.replace(
              regex,
              '<mark style="background-color: yellow;">$1</mark>'
            );
          });
        });
      }, 500);
    };

    loadPDF();
  }, [pdfUrl, currentPage, highlights]);

  // Render page on page change
  useEffect(() => {
    if (!pdf) return;

    const renderPage = async () => {
      const page = await pdf.getPage(currentPage);
      const viewport = page.getViewport({ scale: 1.5 });

      const canvas = canvasRef.current;
      const context = canvas.getContext("2d");
      canvas.height = viewport.height;
      canvas.width = viewport.width;

      await page.render({ canvasContext: context, viewport }).promise;

      const textLayerDiv = textLayerRef.current;
      textLayerDiv.innerHTML = "";
      const textContent = await page.getTextContent();

      pdfjsLib.renderTextLayer({
        textContent,
        container: textLayerDiv,
        viewport,
        textDivs: [],
        enhanceTextSelection: true
      });

      // Re-apply highlights
      setTimeout(() => {
        const savedHighlights = highlights[currentPage] || [];
        savedHighlights.forEach(({ text }) => {
          [...textLayerDiv.childNodes].forEach((child) => {
            const regex = new RegExp(`(${text})`, "gi");
            child.innerHTML = child.textContent.replace(
              regex,
              '<mark style="background-color: yellow;">$1</mark>'
            );
          });
        });
      }, 500);
    };

    renderPage();
  }, [currentPage, pdf, highlights]);

  const handleHighlight = () => {
    const selection = window.getSelection();
    const selectedText = selection.toString().trim();

    if (selectedText.length > 0) {
      const updated = {
        ...highlights,
        [currentPage]: [
          ...(highlights[currentPage] || []),
          { id: Date.now(), text: selectedText }
        ]
      };
      setHighlights(updated);
      localStorage.setItem("pdf-highlights", JSON.stringify(updated));
    }
  };

  const exportHighlights = () => {
    const dataStr = JSON.stringify(highlights, null, 2);
    const blob = new Blob([dataStr], { type: "application/json" });
    const url = URL.createObjectURL(blob);
    const a = document.createElement("a");
    a.href = url;
    a.download = "highlights.json";
    a.click();
  };

  const handleSearch = () => {
    if (!searchText.trim()) return;
    const textLayerDiv = textLayerRef.current;
    const regex = new RegExp(`(${searchText})`, "gi");
    [...textLayerDiv.childNodes].forEach((child) => {
      child.innerHTML = child.textContent.replace(
        regex,
        '<mark style="background-color: lightgreen;">$1</mark>'
      );
    });
  };

  return (
    <div>
      <div style={{ position: "relative" }}>
        <canvas ref={canvasRef} />
        <div
          ref={textLayerRef}
          className="textLayer"
          style={{
            position: "absolute",
            top: 0,
            left: 0,
            right: 0,
            bottom: 0,
            pointerEvents: "auto",
          }}
        />
      </div>

      <div style={{ marginTop: "10px", marginLeft: "45px" }}>
        <button onClick={() => setCurrentPage((p) => Math.max(p - 1, 1))}>
          Prev
        </button>
        <span style={{ margin: "0 10px" }}>
          Page {currentPage} / {totalPages}
        </span>
        <button onClick={() => setCurrentPage((p) => Math.min(p + 1, totalPages))}>
          Next
        </button>
        <button onClick={handleHighlight} style={{ marginLeft: "10px" }}>
          Highlight
        </button>
        <button onClick={exportHighlights} style={{ marginLeft: "10px" }}>
          Export Highlights
        </button>
      </div>

      <div style={{ marginTop: "10px", marginLeft: "45px" }}>
        <input
          type="text"
          placeholder="Search text"
          value={searchText}
          onChange={(e) => setSearchText(e.target.value)}
        />
        <button onClick={handleSearch}>Search</button>
      </div>
    </div>
  );
};

export default PDFHighlighter;
