import React from "react";
import Navbar from "./navbar/Navbar";
import Sidebar from "./sidebar/Sidebar";
import "bootstrap/dist/css/bootstrap.min.css";

const ManageCauseList = () => {
  return (
    <div className="home">
      <Sidebar />
      <div className="homeContainer">
        <Navbar />

        {/* Main Content */}
        {/* Main content area with sidebar spacing */}
      <div   className="container-fluid">
        <div className="card border shadow-sm">
          <div className="card-header bg-dark text-white fw-semibold">
            Manage Cause List
          </div>

          <div className="card-body">
            {/* Filters Row */}
            <div className="row g-3 align-items-center mb-3">
              <div className="col-md-3">
                <select className="form-select">
                  <option>Select List Type</option>
                </select>
              </div>
              <div className="col-md-3">
                <select className="form-select">
                  <option>Select Court</option>
                </select>
              </div>
              <div className="col-md-3">
                <input
                  type="text"
                  className="form-control"
                  value="Tue May 04 2021 12:11:04 GMT+..."
                  disabled
                />
              </div>
              <div className="col-md-3 d-flex justify-content-end gap-2">
                <button className="btn btn-primary btn-sm">Search</button>
                <button className="btn btn-info text-white btn-sm">Download</button>
              </div>
            </div>

            {/* Action Buttons */}
            <div className="mb-3 d-flex gap-2 flex-wrap">
              <button className="btn btn-info text-white btn-sm">Upload CauseList</button>
              <button className="btn btn-info text-white btn-sm">Add CaseToCauseList</button>
              <button className="btn btn-info text-white btn-sm">Add Manual Case</button>
              <button className="btn btn-info text-white btn-sm">Update Court</button>
              <button className="btn btn-danger btn-sm">Delete All</button>
            </div>

            {/* Table */}
            <div className="table-responsive">
              <table className="table table-bordered table-sm text-center align-middle">
                <thead className="table-light">
                  <tr>
                    <th>Sl No.</th>
                    <th>Case No</th>
                    <th>Petitioner vs Respondent</th>
                    <th>Petitioner Council</th>
                    <th>Respondent Council</th>
                    <th>Court No.</th>
                    <th>
                      Select All <input type="checkbox" />
                    </th>
                    <th>Action</th>
                  </tr>
                </thead>
                <tbody>
                  <tr>
                    <td colSpan="8" className="text-muted">
                      No records found.
                    </td>
                  </tr>
                </tbody>
              </table>
            </div>
          </div>
        </div>
      </div>
      </div>
    </div>
  );
};

export default ManageCauseList;
