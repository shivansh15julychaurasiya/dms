import React, { useState, useEffect } from "react";
import {
  Row,
  Col,
  Button,
  Accordion,
  AccordionItem,
  AccordionHeader,
  AccordionBody,
  Table,
} from "reactstrap";
import {
  fetchPdfDetailsFileById,
  fetchOrderReportByFdId,
  showStampReportPdfFile,
} from "../../services/PdfFileService";
import { getOrdersFromElegalix,getOrderFromElegalix } from "../../services/caseTypeService";
import { API_BASE_URL, CASE_FILE_API_PATHS } from "../../utils/constants";
import {fetchPdfFileByName} from "../../services/PdfFileService"

import { useSearchParams } from "react-router-dom";
import { useAuth } from "../../context/AuthContext";

const TreeView =({ setPdfUrl }) => {
  const [searchParams] = useSearchParams();
  const fdId = searchParams.get("id");

  const { token } = useAuth();

  const [elegalixReport, setElegalixReport] = useState([]);
  const [orderReport, setOrderReport] = useState([]);
  const [caseFileDetails, setCaseFileDetails] = useState(null);

  const [openItems, setOpenItems] = useState("");

  // toggle logic for accordion
  const toggle = (id) => {
    if (openItems === id) {
      setOpenItems(""); // collapse if already open
    } else {
      setOpenItems(id);
    }
  };
  const urlBase = "localost:8081/dms/";

  // 1 Fetch Stamp Report + Case File details
  useEffect(() => {
    const fetchData = async () => {
      try {
        const detailsResponse = await fetchPdfDetailsFileById(fdId, token);
        const textData = await detailsResponse.data.text();
        const jsonData = JSON.parse(textData);
        setCaseFileDetails(jsonData);

        const reportData = await fetchOrderReportByFdId(fdId, token);
        console.log("Order Reports:", reportData);
        setOrderReport(reportData || []);
      } catch (err) {
        console.error("Error loading PDF and details:", err);
      }
    };

    if (fdId && token) fetchData();
  }, [fdId, token]);

  // 2 Fetch Elegalix Report
  useEffect(() => {
    const fetchOrders = async () => {
      try {
        const data = await getOrdersFromElegalix(fdId, token); //  use fdId, not 12
        console.log("Elegalix Orders:", data);

        //  check if response is wrapped
        console.log(data);
        setElegalixReport(Array.isArray(data?.data) ? data.data : data || []);
      } catch (err) {
        console.error("Failed to load orders:", err);
      }
    };

    if (fdId && token) fetchOrders();
  }, [fdId, token]);

  return (
    <div>
      {/* Action Buttons */}
      <Row className="mb-3 align-items-center">
        <Col xs="auto">
          <Button color="primary" size="sm">
            View All Orders
          </Button>
        </Col>
        <Col xs="auto">
          <Button color="success" size="sm">
            Download File
          </Button>
        </Col>
      </Row>

      {/* Accordion Section */}
        <div className="mt-4" style={{ width: "100%", maxWidth: "1000px" }}>
        <Accordion
          open={openItems}
          toggle={toggle}
          className="custom-accordion"
        >
          {/* Office Report */}
          {/* Office Report */}
          <AccordionItem>
            <AccordionHeader targetId="0">
              Order / Office Report
            </AccordionHeader>
            <AccordionBody accordionId="0">
              {/* Scrollable wrapper */}
              <div style={{ maxHeight: "300px", overflowY: "auto" }}>
                <Table responsive hover bordered>
                  <thead>
                    <tr>
                      <th>Sr No.</th>
                      <th>Type</th>
                      <th>Description</th>
                    </tr>
                  </thead>
                  <tbody>
                    {/* Stamp Report (OrderReport API) */}
                    {orderReport?.length > 0 &&
                      orderReport.map((item, index) => {
                        let rowClass = "table-warning"; // default
                        if (item.ord_type?.toLowerCase().includes("stamp")) {
                          rowClass = "table-danger"; // red
                        } else if (
                          item.ord_type?.toLowerCase().includes("office")
                        ) {
                          rowClass = "table-warning"; // yellow
                        } else if (
                          item.ord_type?.toLowerCase().includes("order")
                        ) {
                          rowClass = "table-success"; // green
                        }

                        return (
                          <tr key={`stamp-${index}`} className={rowClass}>
                            <td>{index + 1}</td>
                            <td>
                              <span
                                style={{
                                  color: "blue",
                                  fontWeight: "bold",
                                  textDecoration: "underline",
                                  cursor: "pointer",
                                }}
                               onClick={async () => {
  try {
    const blob = await showStampReportPdfFile(item.ordSdMid, token);
    const url = URL.createObjectURL(blob);
    setPdfUrl(url);   // ⬅ single setter now
  } catch (err) {
    console.error("Failed to open PDF", err);
  }
}}

                              >
                                {item.ord_type || "St. Rep."}{" "}
                                {item.ord_created
                                  ? new Date(
                                      item.ord_created
                                    ).toLocaleDateString("en-GB")
                                  : ""}
                              </span>
                              <br />
                              <i className="bi bi-folder-fill me-2"></i>
                              <i className="bi bi-pencil-square"></i>
                            </td>
                            {/* <td>{item.ord_remark || "N/A"}</td> */}
                          </tr>
                        );
                      })}

                    {/* Order Report (Elegalix API) */}
                    {elegalixReport?.length > 0 &&
                      elegalixReport.map((item, index) => {
                          console.log("Rendering row:", index, item);
                        let rowClass = "table-warning"; // default
                        if (
                          item.documentType?.description
                            ?.toLowerCase()
                            .includes("ord")
                        ) {
                          rowClass = "table-success"; // green
                        } else if (
                          item.documentType?.description
                            ?.toLowerCase()
                            .includes("office")
                        ) {
                          rowClass = "table-warning"; // yellow
                        } else {
                          rowClass = "table-info"; // blue
                        }

                        return (
                          <tr key={`ord-${index}`} className={rowClass}>
                            <td>{index + 1}</td>
                            <td>
                              <span
                                style={{
                                  color: "blue",
                                  fontWeight: "bold",
                                  textDecoration: "underline",
                                  cursor: "pointer",
                                }}
                               onClick={async () => {
  try {
    const response = await getOrderFromElegalix(item.judgmentID, token);
    const res1 = await fetchPdfFileByName(response.data.document_name, token);
    const fileURL = URL.createObjectURL(res1);
    setPdfUrl(fileURL);   // ⬅ single setter now
  } catch (err) {
    console.error("Failed to open PDF", err);
  }
}}

                              >
                                {item.documentType?.description || "ORD"}{" "}
                                {item.sd_submitted_date
                                  ? new Date(
                                      item.sd_submitted_date
                                    ).toLocaleDateString("en-GB")
                                  : ""}
                              </span>
                              <br />
                              <i className="bi bi-folder-fill me-2"></i>
                              <i className="bi bi-pencil-square"></i>
                            </td>
                            {/* <td>{item.docName || item.title || "N/A"}</td> */}
                          </tr>
                        );
                      })}

                    {/* If no data in both */}
                    {!orderReport?.length && !elegalixReport?.length && (
                      <tr>
                        <td colSpan="3" className="text-center">
                          No data available
                        </td>
                      </tr>
                    )}
                  </tbody>
                </Table>
              </div>
            </AccordionBody>
          </AccordionItem>

          {/* Petitioners */}
          <AccordionItem>
            <AccordionHeader targetId="1">Petitioners</AccordionHeader>
            <AccordionBody accordionId="1">
              <Table responsive hover>
                <thead>
                  <tr>
                    <th>Sr No.</th>
                    <th>Petitioner Name</th>
                  </tr>
                </thead>
                <tbody>
                  {caseFileDetails?.petitioners?.length > 0 ? (
                    caseFileDetails.petitioners.map((item, index) => (
                      <tr key={index}>
                        <td>{index + 1}</td>
                        <td>{item.pt_name}</td>
                      </tr>
                    ))
                  ) : (
                    <tr>
                      <td colSpan="2" className="text-center">
                        No data available
                      </td>
                    </tr>
                  )}
                </tbody>
              </Table>
            </AccordionBody>
          </AccordionItem>

          {/* Respondents */}
          <AccordionItem>
            <AccordionHeader targetId="2">Respondents</AccordionHeader>
            <AccordionBody accordionId="2">
              <Table responsive hover>
                <thead>
                  <tr>
                    <th>Sr No.</th>
                    <th>Respondent Name</th>
                  </tr>
                </thead>
                <tbody>
                  {caseFileDetails?.respondents?.length > 0 ? (
                    caseFileDetails.respondents.map((item, index) => (
                      <tr key={index}>
                        <td>{index + 1}</td>
                        <td>{item.rt_name}</td>
                      </tr>
                    ))
                  ) : (
                    <tr>
                      <td colSpan="2" className="text-center">
                        No data available
                      </td>
                    </tr>
                  )}
                </tbody>
              </Table>
            </AccordionBody>
          </AccordionItem>
        </Accordion>
      </div>
    </div>
  );
};

export default TreeView;
