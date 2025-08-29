import { axiosInstance } from './userService';  // Correctly import axiosInstance
import {CASE_TYPE_API_PATHS, COURT_MASTER,} from "../utils/constants"
import { CAUSE_LIST_API_PATHS, CASE_FILE_API_PATHS } from '../utils/constants';
import { showAlert } from "../utils/helpers";

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
    const response = await axiosInstance.get(COURT_MASTER.COURT_MASTER_TYPE, {
      headers: {
        Authorization: `Bearer ${token}`,
      },
    });

    const result = response.data;

    if (result.status && Array.isArray(result.data)) {
      return result.data;
    } else {
      console.warn("Unexpected court master response format:", result);
      return []; // fallback to empty array
    }
  } catch (error) {
    console.error("Failed to fetch court master types:", error);
    throw new Error("Unable to load court master types.");
  }
};



// Create new Court Master Type
export const createCourtMasterType = async (data, token) => {
  try {
    const response = await axiosInstance.post(COURT_MASTER.CREATE_NEW_COURT, data, {
      headers: {
        Authorization: `Bearer ${token}`,
        "Content-Type": "application/json",
      },
    });

    // Assuming response format: { status: true, message: "...", data: {...} }
    return response.data.data;
  } catch (error) {
    console.error("Failed to create court master type:", error);
    throw new Error("Unable to create court master type.");
  }
};

//  services/courtService.js
export const updateCourtBenchId = async (id, benchId, token) => {
  const response = await axiosInstance.put(
    `/court-master-type/update-bench`,
    { id, benchId },
    {
      headers: {
        Authorization: `Bearer ${token}`,
        'Content-Type': 'application/json'
      },
    }
  );
  return response.data;
};


// CAUSELIST SEARCH METHOD

export const searchCauseLists = async ( courtNo, listTypeId, dol, token ) => {
  console.log(token+dol)
  try {
    const response = await axiosInstance.get(CAUSE_LIST_API_PATHS.CAUSE_LIST_SEARCH, {
      params: {
        courtNo,
        listTypeId,
        dol,
      },
      headers: {
        Authorization: `Bearer ${token}`,
        'Content-Type': 'application/json',
      },
    });

    console.log(response.data.data);
    return response.data.data;
  } catch (error) {
    console.error("Error fetching cause lists:", error);
    throw error;
  }
};


//  Get Order From Elegalix
export const getOrdersFromElegalix = async (id, token) => {
  try {
    const response = await axiosInstance.get(
      `${CASE_FILE_API_PATHS.GET_ORDERS_FROM_ELEGALIX}/${id}`,
      {
        headers: {
          Authorization: `Bearer ${token}`,
          "Content-Type": "application/json",
        },
      }
    );

    console.log("Orders from Elegalix:", response.data);
    return response.data;
  } catch (error) {
    console.error("Error fetching orders from Elegalix:", error);
    throw error;
  }
};


// Get Order from elegalix by Id
export const getOrderFromElegalix = async (id, token) => {
  console.log("judgment id*******"+id)
  try {
    const response = await axiosInstance.get(CASE_FILE_API_PATHS.GET_ORDER_FROM_ELEGALIX(id), {
      headers: {
        Authorization: `Bearer ${token}`,
      },
      // responseType: "blob", // if API returns a file/PDF
    });
    return response.data;
  } catch (error) {
    console.error("Error fetching order from Elegalix:", error);
    throw error;
  }
};















