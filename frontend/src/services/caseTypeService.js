import { axiosInstance } from './userService';  // Correctly import axiosInstance
import {CASE_TYPE_API_PATHS} from "../utils/constants"
import { CAUSE_LIST_API_PATHS } from '../utils/constants';
// case types services

//  GET ALL CASE TYPES

export const fetchCaseTypes = async (token) => {
  try {
    console.log(token)
    const response = await axiosInstance.get(CASE_TYPE_API_PATHS.CASE_TYPES, {
      headers: {
        Authorization: `Bearer ${token}`,
      },
    });
    console.log(response.data.data)
    return response.data.data; // Assuming backend wraps it inside `data`
  } catch (error) {
    console.error("Failed to fetch case types:", error);
    throw new Error("Unable to load case types.");
  }
};




export const searchDocuments = async (token, caseTypeId, caseNo, caseYear) => {
  try {
    const response = await axiosInstance.get("/api/casesfiles/search", {
      headers: {
        Authorization: `Bearer ${token}`,
      },
      params: {
        caseTypeId,
        caseNo,
        caseYear,
      },
    });

    return response.data.data; // assuming your ApiResponse structure
  } catch (error) {
    console.error("Document search failed:", error);
    throw error;
  }
};

// GET ALL CAUSE LIST TYPES
export const fetchCauseListTypes = async (token) => {
  try {
    const response = await axiosInstance.get(CAUSE_LIST_API_PATHS.CAUSE_LIST_TYPES, {
      headers: {
        Authorization: `Bearer ${token}`,
      },
    });

    return response.data.data; // assuming your ApiResponse<T> wraps response in `data`
  } catch (error) {
    console.error("Failed to fetch cause list types:", error);
    throw new Error("Unable to load cause list types.");
  }
};


//  Get all Court Master Types
export const fetchCourtMasterTypes = async (token) => {
  try {
    const response = await axiosInstance.get(CAUSE_LIST_API_PATHS.COURT_MASTER_TYPE, {
      headers: {
        Authorization: `Bearer ${token}`,
      },
    });

    // Assuming response structure: { status: true, message: "...", data: [...] }
    return response.data.data;
  } catch (error) {
    console.error("Failed to fetch court master types:", error);
    throw new Error("Unable to load court master types.");
  }
};




