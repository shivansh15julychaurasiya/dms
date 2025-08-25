import axios from "axios";
import { showAlert } from "../utils/helpers";
import { API_BASE_URL,CAUSE_LIST_API_PATHS} from "../utils/constants";

const axiosInstance = axios.create({
  baseURL: API_BASE_URL,
  headers: {
    "Content-Type": "application/json",
  },
});

export const getCauseListData = async (token,listTypeId) => {
  console.log(token,listTypeId)
  try {
   // const res = await axiosInstance.get(CAUSE_LIST_API_PATHS.CAUSE_LIST_DATA, {
   const res = await axiosInstance.get(CAUSE_LIST_API_PATHS.CAUSE_LIST_DATA(listTypeId),{
      // `/cause-lists/getCauseList/${listTypeId}`,{
     
      headers: {
        Authorization: `Bearer ${token}`,
      },
    });
    console.log(res.data.data)
   return res.data.data
    //  console.log(res.data.data)
  
  } catch (error) {
    console.error("Failed to fetch roles:", error);
  }
};

export const getCauseListTotal = async ( token) => {
  console.log(token)
  try {
    const res = await axiosInstance.get(CAUSE_LIST_API_PATHS.CAUSE_LIST_TOTAL, {
     
      headers: {
        Authorization: `Bearer ${token}`,
      },
    });

   return res.data.data
     console.log(res.data.data)
  
  } catch (error) {
    console.error("Failed to fetch roles:", error);
  }
};

