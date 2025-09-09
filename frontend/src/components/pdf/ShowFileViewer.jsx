import React, { useState, useEffect, useCallback, useRef } from 'react';
import { Upload, Download, Trash2, Eye, EyeOff, FileText, Bookmark, Highlighter } from 'lucide-react';

// Mock the react-pdf-viewer components for this demo
// In your actual implementation, use the real imports:
// import { Viewer, Worker } from "@react-pdf-viewer/core";
// import { highlightPlugin } from "@react-pdf-viewer/highlight";
// import { toolbarPlugin } from "@react-pdf-viewer/toolbar";
// import { bookmarkPlugin } from "@react-pdf-viewer/bookmark";

// Mock Worker component
const ShowFileViewer = ({ children, workerUrl }) => <div>{children}</div>;

// Mock Viewer component
const Viewer = ({ fileUrl, plugins = [], onDocumentLoad, ...props }) => {
  useEffect(() => {
    // Simulate document load
    if (onDocumentLoad) {
      setTimeout(() => {
        onDocumentLoad({
          doc: {
            getOutline: () => Promise.resolve([{ title: 'Chapter 1' }, { title: 'Chapter 2' }])
          }
        });
      }, 100);
    }
  }, [fileUrl, onDocumentLoad]);

  return (
    <div style={{ 
      width: '100%', 
      height: '100%', 
      border: '1px solid #ddd',
      display: 'flex',
      alignItems: 'center',
      justifyContent: 'center',
      backgroundColor: '#f9f9f9',
      fontSize: '14px',
      color: '#666'
    }}>
      {fileUrl ? (
        <iframe
          src={fileUrl}
          style={{ width: '100%', height: '100%', border: 'none' }}
          title="PDF Viewer"
        />
      ) : (
        'PDF Viewer - Upload a PDF to view'
      )}
    </div>
  );
};

// Mock Toolbar component
const Toolbar = () => (
  <div style={{ 
    padding: '8px', 
    backgroundColor: '#f5f5f5', 
    border: '1px solid #ddd',
    display: 'flex',
    gap: '8px',
    alignItems: 'center',
    fontSize: '12px'
  }}>
    <button style={{ padding: '4px 8px', fontSize: '11px' }}>🔍 Zoom</button>
    <button style={{ padding: '4px 8px', fontSize: '11px' }}>⬅️ Prev</button>
    <span>Page 1 of 10</span>
    <button style={{ padding: '4px 8px', fontSize: '11px' }}>➡️ Next</button>
    <button style={{ padding: '4px 8px', fontSize: '11px' }}>🖨️ Print</button>
  </div>
);

// Mock Bookmarks component
const Bookmarks = ({ bookmarks = [] }) => (
  <div style={{ fontSize: '12px', color: '#ccc' }}>
    {bookmarks.length > 0 ? (
      bookmarks.map((bookmark, index) => (
        <div key={index} style={{ padding: '4px 0', cursor: 'pointer' }}>
          📖 {bookmark.title || `Bookmark ${index + 1}`}
        </div>
      ))
    ) : (
      <div>No bookmarks available</div>
    )}
  </div>
);

const EnhancedPDFViewer = () => {
  const [pdfUrl, setPdfUrl] = useState(null);
  const [allOrderPdf, setAllOrderPdf] = useState([]);
  const [hasBookmarks, setHasBookmarks] = useState(true);
  const [visiblePanel, setVisiblePanel] = useState("bookmark");
  const [activeDoc, setActiveDoc] = useState("pdfUrl");
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState(null);
  const fileInputRef = useRef(null);

  // Highlights state - store highlights for each document
  const [documentHighlights, setDocumentHighlights] = useState({});
  const [currentDocumentId, setCurrentDocumentId] = useState('main');

  // localStorage key for persisting highlights
  const HIGHLIGHTS_STORAGE_KEY = 'pdf-viewer-highlights';

  // Load highlights from localStorage on component mount
  useEffect(() => {
    try {
      // In your actual implementation, uncomment this:
      // const savedHighlights = localStorage.getItem(HIGHLIGHTS_STORAGE_KEY);
      // if (savedHighlights) {
      //   setDocumentHighlights(JSON.parse(savedHighlights));
      // }
    } catch (error) {
      console.error('Error loading highlights from localStorage:', error);
    }
  }, []);

  // Save highlights to localStorage whenever they change
  useEffect(() => {
    try {
      // In your actual implementation, uncomment this:
      // localStorage.setItem(HIGHLIGHTS_STORAGE_KEY, JSON.stringify(documentHighlights));
    } catch (error) {
      console.error('Error saving highlights to localStorage:', error);
    }
  }, [documentHighlights]);

  // Get highlights for current document
  const getCurrentHighlights = useCallback(() => {
    return documentHighlights[currentDocumentId] || [];
  }, [documentHighlights, currentDocumentId]);

  // Add highlight handler
  const handleAddHighlight = useCallback((highlight) => {
    const newHighlight = {
      ...highlight,
      id: Date.now().toString(),
      timestamp: new Date().toISOString(),
      documentId: currentDocumentId,
      note: highlight.annotation?.content || highlight.content || '',
    };

    setDocumentHighlights(prev => ({
      ...prev,
      [currentDocumentId]: [
        ...(prev[currentDocumentId] || []),
        newHighlight
      ]
    }));
  }, [currentDocumentId]);

  // Remove highlight handler
  const handleRemoveHighlight = useCallback((highlightId) => {
    setDocumentHighlights(prev => ({
      ...prev,
      [currentDocumentId]: (prev[currentDocumentId] || []).filter(
        h => h.id !== highlightId
      )
    }));
  }, [currentDocumentId]);

  // Clear all highlights for current document
  const handleClearAllHighlights = useCallback(() => {
    setDocumentHighlights(prev => ({
      ...prev,
      [currentDocumentId]: []
    }));
  }, [currentDocumentId]);

  // Export highlights
  const exportHighlights = () => {
    const allHighlights = Object.keys(documentHighlights).reduce((acc, docId) => {
      acc[docId] = documentHighlights[docId];
      return acc;
    }, {});

    const dataStr = JSON.stringify(allHighlights, null, 2);
    const dataBlob = new Blob([dataStr], { type: 'application/json' });
    const url = URL.createObjectURL(dataBlob);
    const link = document.createElement('a');
    link.href = url;
    link.download = `pdf-highlights-${new Date().toISOString().split('T')[0]}.json`;
    link.click();
    URL.revokeObjectURL(url);
  };

  // Import highlights
  const importHighlights = (event) => {
    const file = event.target.files[0];
    if (file) {
      const reader = new FileReader();
      reader.onload = (e) => {
        try {
          const imported = JSON.parse(e.target.result);
          setDocumentHighlights(prev => ({
            ...prev,
            ...imported
          }));
        } catch (error) {
          alert('Error importing highlights: Invalid file format');
        }
      };
      reader.readAsText(file);
    }
  };

  // Mock highlight plugin (in real implementation, use actual highlightPlugin)
  const createHighlightPlugin = () => ({
    highlights: getCurrentHighlights(),
    
    renderHighlightTarget: (props) => (
      <div
        style={{
          background: '#FFE066',
          border: '1px solid #FFD700',
          borderRadius: '4px',
          padding: '8px 12px',
          boxShadow: '0 2px 4px rgba(0,0,0,0.1)',
          cursor: 'pointer',
          fontSize: '14px',
          fontWeight: 'bold',
          color: '#333',
          zIndex: 1000,
        }}
        onClick={() => {
          const note = window.prompt('Add a note for this highlight (optional):') || '';
          handleAddHighlight({
            ...props.highlight,
            annotation: { content: note },
            content: note
          });
          props.onConfirm && props.onConfirm(note);
        }}
      >
        📝 Add Highlight
      </div>
    ),

    renderHighlightContent: (props) => {
      const { highlight } = props;
      const note = highlight.annotation?.content || highlight.content || highlight.note || '';
      
      return (
        <div
          style={{
            background: '#fff',
            border: '1px solid #ccc',
            borderRadius: '4px',
            padding: '8px',
            boxShadow: '0 2px 8px rgba(0,0,0,0.15)',
            maxWidth: '200px',
            fontSize: '12px',
            zIndex: 1001,
          }}
        >
          {note ? (
            <>
              <div style={{ fontWeight: 'bold', marginBottom: '4px', color: '#333' }}>
                📝 Note:
              </div>
              <div style={{ color: '#666', marginBottom: '8px' }}>
                {note}
              </div>
            </>
          ) : (
            <div style={{ color: '#666', marginBottom: '8px' }}>
              Highlight (no note)
            </div>
          )}
          <button
            style={{
              background: '#ff4444',
              color: 'white',
              border: 'none',
              borderRadius: '2px',
              padding: '2px 6px',
              fontSize: '11px',
              cursor: 'pointer',
            }}
            onClick={() => handleRemoveHighlight(highlight.id)}
          >
            Remove
          </button>
        </div>
      );
    },

    onAddHighlight: handleAddHighlight,
  });

  // Handle file upload
  const handleFileUpload = (event) => {
    const file = event.target.files[0];
    if (file && file.type === 'application/pdf') {
      const url = URL.createObjectURL(file);
      setPdfUrl(url);
      setCurrentDocumentId('main');
      setActiveDoc('pdfUrl');
    } else {
      alert('Please select a valid PDF file');
    }
  };

  // Handle document load
  const handleDocumentLoad = async (e, documentIndex = null) => {
    try {
      const outline = await e.doc.getOutline();
      setHasBookmarks(outline && outline.length > 0);
      
      if (documentIndex !== null) {
        setCurrentDocumentId(`doc-${documentIndex}`);
      }
    } catch (err) {
      console.error("Error loading document outline:", err);
    }
  };

  // Render highlights list
  const renderHighlightsList = () => {
    const highlights = getCurrentHighlights();
    
    if (highlights.length === 0) {
      return (
        <div style={{ padding: '10px', color: '#666', fontSize: '12px' }}>
          No highlights yet. Select text in the PDF to add highlights.
        </div>
      );
    }

    return (
      <div style={{ padding: '10px' }}>
        <div style={{ 
          display: 'flex', 
          justifyContent: 'space-between', 
          alignItems: 'center',
          marginBottom: '10px' 
        }}>
          <h6 style={{ color: '#fff', margin: 0 }}>
            📝 Highlights ({highlights.length})
          </h6>
          <button
            onClick={handleClearAllHighlights}
            style={{
              background: '#ff4444',
              color: 'white',
              border: 'none',
              borderRadius: '3px',
              padding: '2px 6px',
              fontSize: '10px',
              cursor: 'pointer',
            }}
            title="Clear all highlights"
          >
            Clear All
          </button>
        </div>
        
        <div style={{ maxHeight: '300px', overflowY: 'auto' }}>
          {highlights.map((highlight, index) => (
            <div
              key={highlight.id || index}
              style={{
                background: '#333',
                border: '1px solid #555',
                borderRadius: '4px',
                padding: '8px',
                marginBottom: '8px',
                fontSize: '11px',
              }}
            >
              <div style={{ 
                color: '#FFE066', 
                marginBottom: '4px',
                display: 'flex',
                justifyContent: 'space-between',
                alignItems: 'center'
              }}>
                <span>Page {(highlight.pageIndex || 0) + 1}</span>
                <span style={{ fontSize: '9px', color: '#999' }}>
                  {highlight.timestamp ? new Date(highlight.timestamp).toLocaleDateString() : ''}
                </span>
              </div>
              
              {highlight.note || highlight.content ? (
                <div style={{ color: '#ccc', marginBottom: '4px' }}>
                  "{highlight.note || highlight.content}"
                </div>
              ) : (
                <div style={{ color: '#999', marginBottom: '4px' }}>
                  Text highlight (no note)
                </div>
              )}
              
              <button
                style={{
                  background: '#ff4444',
                  color: 'white',
                  border: 'none',
                  borderRadius: '2px',
                  padding: '2px 6px',
                  fontSize: '10px',
                  cursor: 'pointer',
                  marginTop: '4px',
                }}
                onClick={() => handleRemoveHighlight(highlight.id)}
              >
                Remove
              </button>
            </div>
          ))}
        </div>
      </div>
    );
  };

  // Get total highlights count across all documents
  const getTotalHighlights = () => {
    return Object.values(documentHighlights).reduce((total, highlights) => {
      return total + highlights.length;
    }, 0);
  };

  return (
    <div style={{ display: 'flex', height: '100vh', fontFamily: 'Arial, sans-serif' }}>
      {/* Sidebar */}
      <div style={{
        width: '320px',
        backgroundColor: '#2c3e50',
        color: 'white',
        display: 'flex',
        flexDirection: 'column',
        borderRight: '1px solid #34495e'
      }}>
        {/* Header */}
        <div style={{ 
          padding: '16px', 
          borderBottom: '1px solid #34495e',
          backgroundColor: '#34495e'
        }}>
          <h3 style={{ margin: 0, display: 'flex', alignItems: 'center', gap: '8px' }}>
            <FileText size={20} />
            PDF Highlighter Pro
          </h3>
        </div>

        {/* File Upload Section */}
        <div style={{ padding: '16px', borderBottom: '1px solid #34495e' }}>
          <button
            onClick={() => fileInputRef.current.click()}
            style={{
              width: '100%',
              padding: '12px',
              backgroundColor: '#3498db',
              color: 'white',
              border: 'none',
              borderRadius: '6px',
              cursor: 'pointer',
              display: 'flex',
              alignItems: 'center',
              justifyContent: 'center',
              gap: '8px',
              fontSize: '14px',
              fontWeight: '500'
            }}
          >
            <Upload size={16} />
            Upload PDF
          </button>
          <input
            ref={fileInputRef}
            type="file"
            accept=".pdf"
            onChange={handleFileUpload}
            style={{ display: 'none' }}
          />
        </div>

        {/* Stats */}
        <div style={{ 
          padding: '12px 16px', 
          fontSize: '12px', 
          color: '#bdc3c7',
          borderBottom: '1px solid #34495e'
        }}>
          <div>Total Highlights: {getTotalHighlights()}</div>
          <div>Current Doc Highlights: {getCurrentHighlights().length}</div>
        </div>

        {/* Panel Toggle */}
        <div style={{ padding: '12px 16px', borderBottom: '1px solid #34495e' }}>
          <div style={{ display: 'flex', gap: '8px' }}>
            <button
              onClick={() => setVisiblePanel('bookmark')}
              style={{
                flex: 1,
                padding: '8px 12px',
                backgroundColor: visiblePanel === 'bookmark' ? '#3498db' : '#34495e',
                color: 'white',
                border: 'none',
                borderRadius: '4px',
                cursor: 'pointer',
                fontSize: '12px',
                display: 'flex',
                alignItems: 'center',
                justifyContent: 'center',
                gap: '4px'
              }}
            >
              <Bookmark size={14} />
              Index
            </button>
            <button
              onClick={() => setVisiblePanel('highlights')}
              style={{
                flex: 1,
                padding: '8px 12px',
                backgroundColor: visiblePanel === 'highlights' ? '#3498db' : '#34495e',
                color: 'white',
                border: 'none',
                borderRadius: '4px',
                cursor: 'pointer',
                fontSize: '12px',
                display: 'flex',
                alignItems: 'center',
                justifyContent: 'center',
                gap: '4px'
              }}
            >
              <Highlighter size={14} />
              Notes
            </button>
          </div>
        </div>

        {/* Export/Import Controls */}
        <div style={{ padding: '12px 16px', borderBottom: '1px solid #34495e' }}>
          <div style={{ display: 'flex', gap: '8px', marginBottom: '8px' }}>
            <button
              onClick={exportHighlights}
              disabled={getTotalHighlights() === 0}
              style={{
                flex: 1,
                padding: '6px 8px',
                backgroundColor: getTotalHighlights() > 0 ? '#27ae60' : '#7f8c8d',
                color: 'white',
                border: 'none',
                borderRadius: '4px',
                cursor: getTotalHighlights() > 0 ? 'pointer' : 'not-allowed',
                fontSize: '11px',
                display: 'flex',
                alignItems: 'center',
                justifyContent: 'center',
                gap: '4px'
              }}
            >
              <Download size={12} />
              Export
            </button>
          </div>
          <input
            type="file"
            accept=".json"
            onChange={importHighlights}
            style={{ 
              width: '100%', 
              fontSize: '11px',
              padding: '4px',
              backgroundColor: '#34495e',
              color: 'white',
              border: '1px solid #7f8c8d',
              borderRadius: '4px'
            }}
          />
        </div>

        {/* Panel Content */}
        <div style={{ flex: 1, overflow: 'auto' }}>
          {visiblePanel === 'bookmark' && (
            <div style={{ padding: '16px' }}>
              <h6 style={{ color: '#fff', marginBottom: '12px', fontSize: '14px' }}>
                📚 Document Index
              </h6>
              {hasBookmarks ? (
                <Bookmarks bookmarks={[
                  { title: 'Introduction' },
                  { title: 'Chapter 1' },
                  { title: 'Chapter 2' },
                  { title: 'Conclusion' }
                ]} />
              ) : (
                <div style={{ fontSize: '12px', color: '#bdc3c7' }}>
                  No bookmarks available in this document
                </div>
              )}
            </div>
          )}
          
          {visiblePanel === 'highlights' && renderHighlightsList()}
        </div>
      </div>

      {/* Main Content */}
      <div style={{ flex: 1, display: 'flex', flexDirection: 'column' }}>
        {/* Toolbar */}
        <div style={{ 
          padding: '12px 16px', 
          backgroundColor: '#ecf0f1',
          borderBottom: '1px solid #bdc3c7',
          display: 'flex',
          justifyContent: 'space-between',
          alignItems: 'center'
        }}>
          <Toolbar />
          <div style={{ fontSize: '12px', color: '#7f8c8d' }}>
            Document: {currentDocumentId} | Highlights: {getCurrentHighlights().length}
          </div>
        </div>

        {/* PDF Viewer */}
        <div style={{ flex: 1, overflow: 'hidden' }}>
          {pdfUrl ? (
            <Worker workerUrl="https://unpkg.com/pdfjs-dist@3.11.174/build/pdf.worker.min.js">
              <div style={{ height: '100%' }}>
                <Viewer
                  fileUrl={pdfUrl}
                  plugins={[createHighlightPlugin()]}
                  onDocumentLoad={handleDocumentLoad}
                />
              </div>
            </Worker>
          ) : (
            <div style={{
              height: '100%',
              display: 'flex',
              alignItems: 'center',
              justifyContent: 'center',
              backgroundColor: '#f8f9fa',
              flexDirection: 'column',
              gap: '16px'
            }}>
              <FileText size={64} color="#bdc3c7" />
              <div style={{ textAlign: 'center' }}>
                <h3 style={{ color: '#7f8c8d', margin: 0, marginBottom: '8px' }}>
                  No PDF Loaded
                </h3>
                <p style={{ color: '#95a5a6', margin: 0, fontSize: '14px' }}>
                  Upload a PDF file to start viewing and highlighting
                </p>
              </div>
              <button
                onClick={() => fileInputRef.current.click()}
                style={{
                  padding: '12px 24px',
                  backgroundColor: '#3498db',
                  color: 'white',
                  border: 'none',
                  borderRadius: '6px',
                  cursor: 'pointer',
                  fontSize: '14px'
                }}
              >
                Choose PDF File
              </button>
            </div>
          )}
        </div>
      </div>
    </div>
  );
};

export default ShowFileViewer;