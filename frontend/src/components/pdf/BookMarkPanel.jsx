import React from 'react';

const BookMarkPanel = ({ renderBookmarks, fileUrl }) => {
    console.log(fileUrl)
    if (!renderBookmarks) return <div>Loading bookmarks...</div>;

    return (
        <div style={{ padding: '1rem' }}>
            <h5>Bookmarks</h5>
            {renderBookmarks({ fileUrl })}
        </div>
    );
};

export default BookMarkPanel;
