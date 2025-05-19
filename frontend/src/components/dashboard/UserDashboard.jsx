import React, { useEffect, useState } from "react";
import Sidebar from "../layout/Sidebar";
import Navbar from "../layout/Navbar";
import "../../assets/styles.css";
import { useNavigate } from "react-router-dom";
import { fetchUsers, isTokenExpired } from "../../services/userService";

const UserDashboard = () => {
  const [users, setUsers] = useState([]);
  const [error, setError] = useState("");
  const [currentPage, setCurrentPage] = useState(1);

  const navigate = useNavigate();
  const usersPerPage = 5;

  useEffect(() => {
    const checkTokenAndFetch = () => {
      const tokenLog = localStorage.getItem("tokenLog");
      if (!tokenLog || isTokenExpired(tokenLog)) {
        localStorage.removeItem("tokenLog");
        navigate("/home/login");
      } else {
        fetchUsers(setUsers, setError, navigate);
      }
    };



    checkTokenAndFetch();
    const intervalId = setInterval(() => {
      const tokenLog = localStorage.getItem("tokenLog");
      if (!tokenLog || isTokenExpired(tokenLog)) {
        localStorage.removeItem("tokenLog");
        navigate("/home/login");
      }
    }, 10000);

    return () => clearInterval(intervalId);
  }, []);


  const indexOfLastUser = currentPage * usersPerPage;
  const indexOfFirstUser = indexOfLastUser - usersPerPage;
  const currentUsers = users.slice(indexOfFirstUser, indexOfLastUser);
  const totalPages = Math.ceil(users.length / usersPerPage);

  const paginate = (pageNumber) => setCurrentPage(pageNumber);
  const nextPage = () => setCurrentPage((prev) => Math.min(prev + 1, totalPages));
  const prevPage = () => setCurrentPage((prev) => Math.max(prev - 1, 1));

  return (
    <div className="home">
      <Sidebar />
      <div className="homeContainer">
        <Navbar />
        <div className="container-fluid p-4">
          <div className="d-flex justify-content-between align-items-center mb-3">
            <h4 className="shimmer-text">Welcome To User Dashboard</h4>
          </div>

          {error && <div className="alert alert-danger">{error}</div>}

          <div className="table-responsive">
            <table className="table table-striped table-hover table-bordered">
              <thead className="table-dark">
                <tr>
                  <th>#</th>
                  <th>Name</th>
                  <th>Email</th>
                  <th>Employee ID</th>
                  <th>Phone</th>
                  <th>About</th>
                  <th>Role</th>
                </tr>
              </thead>
              <tbody>
                {currentUsers.length > 0 ? (
                  currentUsers.map((user, index) => (
                    <tr key={user.userId}>
                      <td>{indexOfFirstUser + index + 1}</td>
                      <td>{user.name}</td>
                      <td>{user.email}</td>
                      <td>{user.login_id}</td>
                      <td>{user.phone}</td>
                      <td>{user.about}</td>
                      <td>
                        {user.roles && user.roles.length > 0 ? (
                          user.roles.map((role, idx) => (
                            <span key={role.id}>
                              {role.name.replace("ROLE_", "")}
                              {idx < user.roles.length - 1 && ", "}
                            </span>
                          ))
                        ) : (
                          <span className="text-muted">No role</span>
                        )}
                      </td>
                    </tr>
                  ))
                ) : (
                  <tr>
                    <td colSpan="8" className="text-center">
                      No users found
                    </td>
                  </tr>
                )}
              </tbody>
            </table>
          </div>

          {totalPages > 1 && (
            <nav>
              <ul className="pagination justify-content-center">
                <li className={`page-item ${currentPage === 1 ? "disabled" : ""}`}>
                  <button className="page-link" onClick={prevPage}>Previous</button>
                </li>
                {[...Array(totalPages)].map((_, i) => (
                  <li
                    key={i + 1}
                    className={`page-item ${currentPage === i + 1 ? "active" : ""}`}
                  >
                    <button className="page-link" onClick={() => paginate(i + 1)}>{i + 1}</button>
                  </li>
                ))}
                <li className={`page-item ${currentPage === totalPages ? "disabled" : ""}`}>
                  <button className="page-link" onClick={nextPage}>Next</button>
                </li>
              </ul>
            </nav>
          )}
        </div>
      </div>
    </div>
  );
};

export default UserDashboard;
