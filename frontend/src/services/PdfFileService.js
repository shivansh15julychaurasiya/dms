import {API_BASE_URL} from "../utils/constants"
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