import React from 'react';
import PDFViewer from './PDFViewerHightLight';
import PDFViewerHightLight from './PDFViewerHightLight';

const App = () => {
    const fileUrl = 'http://localhost:8081/dms/api/casesfiles/documents/view/CLRE362018_PETN_1';
    const fileName = 'CLRE362018_PETN_1'; // Used as key for localStorage

    return (
        <div>
            <PDFViewerHightLight fileUrl={fileUrl} fileName={fileName} />
        </div>
    );
};

export default App;
