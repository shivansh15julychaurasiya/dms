// import React, { useState } from 'react';
import Sidebar from "./sidebar/Sidebar"; // Make sure Sidebar is in the same folder or adjust path
import "bootstrap/dist/css/bootstrap.min.css";
import Navbar from "./navbar/Navbar";
import "../Pages/home/home.scss";

const Search = () => {
  //   const [searchTerm, setSearchTerm] = useState('');

  //   const data = [
  //     { id: 1, name: 'John Doe', role: 'Lawyer' },
  //     { id: 2, name: 'Jane Smith', role: 'Judge' },
  //     { id: 3, name: 'Michael Lee', role: 'Clerk' },
  //     { id: 4, name: 'Samantha Brown', role: 'Lawyer' },
  //   ];

  //   const filteredData = data.filter((item) =>
  //     item.name.toLowerCase().includes(searchTerm.toLowerCase())
  //   );

  return (
    <div className="home">
      <Sidebar />
      <div className="homeContainer">
        <Navbar />

        {/* Main Content */}
        <div className="container-fluid p-4 ">
          <div className="header bg-dark mt-2 py-2 rounded">
            <h className="text-light  mt-2 mb-2 px-3">Search Cause List</h>
          </div>

          <div class="container  mt-4">
            <form>
              <div class="row g-3 align-items-center ">
                {/* <!-- Select Type --> */}
                <div class="col-md-3 col-sm-6">
                  <select id="typeSelect" class="form-select">
                    <option value="">Select List Type...</option>
                    <option value="civil">Civil</option>
                    <option value="criminal">Criminal</option>
                    <option value="family">Family</option>
                  </select>
                </div>

                {/* <!-- Checkbox --> */}
                <div class="col-md-3 col-sm-6 d-flex align-items-center">
                  <div class="form-check mt-2">
                    <input
                      class="form-check-input"
                      type="checkbox"
                      id="caseCheck"
                    />
                    <label class="form-check-label" for="caseCheck">
                      Transferred
                    </label>
                  </div>
                </div>

                {/* Date picker */}
                <div class="col-md-3 col-sm-6">
                  <input type="date" id="dateInput" class="form-control" />
                </div>

                {/* <!-- Submit Button --> */}
                <div class="col-md-3 col-sm-6 px-5 mt-3">
                  <button type="submit" class="btn btn-success">
                    Submit
                  </button>
                </div>
              </div>
            </form>
          </div>

          {/* Table */}
          <div className="table-responsive mt-3 rounded">
            <table className="table table-bordered table-striped rounded">
              <thead className="table-secondary rounded text-center ">
                <tr>
                  <th>Sr No</th>
                  <th>Case No</th>
                  <th>Petitioners vs Responders</th>
                  <th>Petitioner Council</th>
                  <th>Petitioner Council</th>
                </tr>
              </thead>
              <tbody>
                <tr>
                  <td>1</td>
                  <td>232</td>
                  <td>Lorem ipsum dolor sit amet consectetur adipisicing elit. Quod, suscipit.</td>
                  <td>Lorem ipsum dolor sit, amet consectetur adipisicing elit.</td>
                  <td>Lorem ipsum dolor sit amet.</td>
                </tr>
              </tbody>
            </table>
          </div>
        </div>
      </div>
    </div>
  );
};

export default Search;
