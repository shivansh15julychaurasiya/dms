import axios from "axios";
import { showAlert } from "../utils/helpers";
import { API_BASE_URL, API_MANAGE_BENCH } from "../utils/constants";

const axiosInstance = axios.create({
  baseURL: API_BASE_URL,
  headers: {
    "Content-Type": "application/json",
  },
});

export const fetchCourtMaster = async(token) =>{
    const response = await axiosInstance.get(API_MANAGE_BENCH.COURT,{
        headers:{
            Authorization: `Bearer ${token}`
        },
    });
    // console.log(response.data)
    // console.log(response)

    return response;   
}

export const fetchCaseTypes = async(token) =>{
    const response = await axiosInstance.get(API_CASE_TYPES.CASE_TYPES,{
        headers:{
            Authorization: `Bearer ${token}`
        },
    });
    // console.log(response.data)
    // console.log(response)

    return response;   
}
