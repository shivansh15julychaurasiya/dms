import { axiosInstance } from './userService';  // Correctly import axiosInstance
import {CASE_TYPE_API_PATHS} from "../utils/constants"
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

