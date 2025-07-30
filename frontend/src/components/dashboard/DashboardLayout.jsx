import React from "react";
// import { PDFViewer } from "@react-pdf/renderer";
// import PdfComponent from './PdfComponent';
import Navbar from "../layout/Navbar";
// import MyDocument
import FancyAccordion from "../../pages/FancyAccordion";
// import PDFHighlighter from "./pdf/PDFHighlighter";
import PDFViewer from "../pdf/PDFViewerHightLight";

const DashboardLayout = () => {
  return (
    <div className="flex">
      <Navbar />
      <div className="container-fluid mt-4">
        <div className="row ">
          {/* Index Column */}
          <div className="col-md-2">
            <div className="card justify-content-center text-center ">
              <ul className="list-unstyled">
                <li> Case Documents</li>
                <li> Court Orders</li>
                <li> Hearing Schedule</li>
                <li> Client Details</li>
                <ul className="list-unstyled">
                  <li> Case Files</li>
                  <li> Judge Details</li>
                  <li>Client Info</li>
                  <li>Schedule</li>
                  <li> Notifications</li>
                  <li> Notes</li>
                  <li> Reports</li>
                  <li> Court Details</li>
                  <li> History</li>
                  <li>⬇ Downloads</li>
                </ul>
              </ul>
            </div>
          </div>

          {/* List Column */}
          <div className="col-md-4">
            <div className="card">
              <font className="font-weight-bold text-dark mt-2 text-center">
                NABAIL NO 20232 OF 2025
              </font>
              <div className="container my-3">
                <div className="row g-2 justify-content-start flex-wrap">
                  <div className="col-auto">
                    <button className="btn btn-success btn-sm">
                      Stamp Report
                    </button>
                  </div>

                  <div className="col-auto">
                    <button className="btn btn-success btn-sm">Notes</button>
                  </div>

                  <div className="col-auto">
                    <button className="btn btn-success btn-sm">
                      View Orders
                    </button>
                  </div>

                  <div className="col-auto">
                    <button className="btn btn-success btn-sm">Reserved</button>
                  </div>

                  <div className="col-auto">
                    <button className="btn btn-success btn-sm">Download</button>
                  </div>
                </div>
              </div>
            </div>
            <FancyAccordion />
          </div>

          {/* PDF Preview Column */}
          <div className="col-md-6">
          {/* <PDFHighlighter pdfUrl="/sample.pdf" /> */}
          {/* <PDFViewer/> */}
          

          </div>
        </div>
      </div>
    </div>
  );
};

export default DashboardLayout;
