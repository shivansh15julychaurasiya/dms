import {API_BASE_URL,CASE_FILE_API_PATHS} from "../utils/constants"
import { axiosInstance } from './userService'; 

//  GET PDF by Id

export const getDocumentFileUrl = (id) => {
  return `${API_BASE_URL}/api/casesfiles/documents/view/${id}`;
};

export const fetchPdfDetailsFileById = async (id, token) => {
  try {
    const response = await axiosInstance.get(`${API_BASE_URL}/api/casesfiles/documents/view/${id}`, {
      headers: {
        Authorization: `Bearer ${token}`,
      },
      responseType: "blob", // necessary for downloading/viewing binary files like PDFs
    });

    return response; // contains the Blob
  } catch (error) {
    console.error("Failed to fetch document PDF:", error);
    throw new Error("Unable to fetch document PDF.");
  }
};

// Get Case File Pdf file by By Id
export const fetchPdfFileById = async (id, token) => {
  try {
    const response = await axiosInstance.get(`${API_BASE_URL}/api/casesfiles/view/${id}`, {
      headers: {
        Authorization: `Bearer ${token}`,
      },
      responseType: "blob", // necessary for downloading/viewing binary files like PDFs
    });

    return response; // contains the Blob
  } catch (error) {
    console.error("Failed to fetch document PDF:", error);
    throw new Error("Unable to fetch document PDF.");
  }
};

// Fetch Stamp report By Id
export const fetchOrderReportByFdId = async (fdId, token) => {
  try {
    const response = await axiosInstance.get(`${API_BASE_URL}/orderreport/view/${fdId}`, {
      headers: {
        Authorization: `Bearer ${token}`,
      },
    });

    return response.data; // returns the order report data
  } catch (error) {
    console.error("Failed to fetch order report:", error);
    throw new Error("Unable to fetch order report.");
  }
};

// Show stamp report pdf by ID
export const showStampReportPdfFile = async (docId, token) => {
  try {
    const response = await axiosInstance.get(
      CASE_FILE_API_PATHS.SHOW_FILE(docId), //  already adds the docId
      {
        headers: {
          Authorization: `Bearer ${token}`,
        },
        responseType: "blob", // important for PDF files
      }
    );

    return response.data; // returns PDF blob
  } catch (error) {
    console.error("Failed to fetch case file PDF:", error);
    throw new Error("Unable to fetch case file PDF.");
  }
};
export const fetchPdfFileByName = async (fileName, token) => {
  try {
    const response = await axiosInstance.get(`/api/casesfiles/getPdf/${fileName}`, {
      headers: {
        Authorization: `Bearer ${token}`,
      },
      responseType: "blob", // VERY IMPORTANT for PDF download
    });

    return response.data; // returns PDF blob
  } catch (error) {
    console.error("Error fetching PDF:", error);
    throw error;
  }
};

