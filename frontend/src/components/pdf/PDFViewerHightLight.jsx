import React, { useEffect, useRef, useState } from 'react';
import { Viewer, Worker } from '@react-pdf-viewer/core';
import { defaultLayoutPlugin } from '@react-pdf-viewer/default-layout';
import { highlightPlugin } from '@react-pdf-viewer/highlight';

import '@react-pdf-viewer/core/lib/styles/index.css';
import '@react-pdf-viewer/default-layout/lib/styles/index.css';
import '@react-pdf-viewer/highlight/lib/styles/index.css';

const PDFViewerHighlight = ({ fileUrl, fileName }) => {
    const [highlightAreas, setHighlightAreas] = useState([]);

    useEffect(() => {
        const saved = localStorage.getItem(`pdf-highlights-${fileName}`);
        if (saved) {
            setHighlightAreas(JSON.parse(saved));
        }
    }, [fileName]);

    const highlightPluginInstance = highlightPlugin({
        highlightAreas,
        onHighlightSelection: (highlight) => {
            const newAreas = [...highlightAreas, highlight];
            setHighlightAreas(newAreas);
            localStorage.setItem(`pdf-highlights-${fileName}`, JSON.stringify(newAreas));
        },
    });

    const defaultLayoutPluginInstance = defaultLayoutPlugin({
        sidebarTabs: (defaultTabs) => [
            ...defaultTabs,
            {
                content: <div style={{ padding: '1rem' }}>Custom tab or help text here</div>,
                icon: <span>📌</span>,
                title: 'Notes',
            },
        ],
    });

    return (
        <Worker workerUrl="https://unpkg.com/pdfjs-dist@3.11.174/build/pdf.worker.min.js">
            <div style={{ height: '84vh',width:'128vh' }}>
                <Viewer
                    fileUrl={fileUrl}
                    plugins={[highlightPluginInstance, defaultLayoutPluginInstance]}
                />
            </div>
        </Worker>
    );
};

export default PDFViewerHighlight;
