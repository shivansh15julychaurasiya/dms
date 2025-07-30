// import React, { useEffect, useRef } from 'react';
// import { Viewer, Worker } from '@react-pdf-viewer/core';
// import { defaultLayoutPlugin } from '@react-pdf-viewer/default-layout';
// import { highlightPlugin } from '@react-pdf-viewer/highlight';

// import '@react-pdf-viewer/core/lib/styles/index.css';
// import '@react-pdf-viewer/default-layout/lib/styles/index.css';
// import '@react-pdf-viewer/highlight/lib/styles/index.css';

// const PDFViewerHighlight = ({ fileUrl, fileName }) => {
//     const highlightRef = useRef([]);

//     useEffect(() => {
//         const saved = localStorage.getItem(`pdf-highlights-${fileName}`);
//         if (saved) {
//             try {
//                 highlightRef.current = JSON.parse(saved);
//             } catch {
//                 highlightRef.current = [];
//             }
//         }
//     }, [fileName]);

//     const highlightPluginInstance = highlightPlugin({
//         highlightAreas: highlightRef.current,
//         onHighlightSelection: (highlight) => {
//             const updated = [...highlightRef.current, highlight];
//             highlightRef.current = updated;
//             localStorage.setItem(`pdf-highlights-${fileName}`, JSON.stringify(updated));
//         },
//     });

//     const defaultLayoutPluginInstance = defaultLayoutPlugin({
//         sidebarTabs: (defaultTabs) => [
//             ...defaultTabs,
//             {
//                 content: <div style={{ padding: '1rem' }}>📝 My Notes Tab</div>,
//                 icon: <span>🧾</span>,
//                 title: 'Notes',
//             },
//         ],
//     });

//     return (
//         <Worker workerUrl="https://unpkg.com/pdfjs-dist@3.11.174/build/pdf.worker.min.js">
//             <div style={{ height: '95vh' }}>
//                 <Viewer
//                     fileUrl={fileUrl}
//                     plugins={[highlightPluginInstance, defaultLayoutPluginInstance]}
//                 />
//             </div>
//         </Worker>
//     );
// };

// export default PDFViewerHighlight;


import React, { useEffect, useState } from 'react';
import { Viewer, Worker } from '@react-pdf-viewer/core';
import { defaultLayoutPlugin } from '@react-pdf-viewer/default-layout';
import { highlightPlugin } from '@react-pdf-viewer/highlight';
import { bookmarkPlugin } from '@react-pdf-viewer/bookmark';

import '@react-pdf-viewer/core/lib/styles/index.css';
import '@react-pdf-viewer/default-layout/lib/styles/index.css';
import '@react-pdf-viewer/highlight/lib/styles/index.css';
import '@react-pdf-viewer/bookmark/lib/styles/index.css';

const PDFViewerHighlight = ({ fileUrl, fileName }) => {
    const [highlightAreas, setHighlightAreas] = useState([]);

    const highlightPluginInstance = highlightPlugin({
        highlightAreas,
        onHighlightSelection: (highlight) => {
            const newAreas = [...highlightAreas, highlight];
            setHighlightAreas(newAreas);
            localStorage.setItem(`pdf-highlights-${fileName}`, JSON.stringify(newAreas));
        },
    });

    const bookmarkPluginInstance = bookmarkPlugin();
    const Bookmarks = bookmarkPluginInstance.Bookmarks;

    const defaultLayoutPluginInstance = defaultLayoutPlugin();

    useEffect(() => {
        const saved = localStorage.getItem(`pdf-highlights-${fileName}`);
        if (saved) {
            setHighlightAreas(JSON.parse(saved));
        }
    }, [fileName]);

    return (
        <Worker workerUrl="https://unpkg.com/pdfjs-dist@3.11.174/build/pdf.worker.min.js">
            <div style={{ display: 'flex', height: '100vh' }}>
                <div style={{ width: '300px', borderRight: '1px solid #ccc', padding: '1rem', overflowY: 'auto' }}>
                    <h4>Bookmarks</h4>
                    {Bookmarks && <Bookmarks />}
                </div>
                <div style={{ flexGrow: 1 }}>
                    <Viewer
                        fileUrl={fileUrl}
                        plugins={[highlightPluginInstance, bookmarkPluginInstance, defaultLayoutPluginInstance]}
                    />
                </div>
            </div>
        </Worker>
    );
};

export default PDFViewerHighlight;

