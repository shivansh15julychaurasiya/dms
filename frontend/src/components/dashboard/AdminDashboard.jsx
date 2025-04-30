import React, { useEffect, useState } from "react";
import {
  Button,
  Container,
  Row,
  Col,
  Nav,
  NavItem,
  NavLink,
  TabContent,
  TabPane,
  Card,
  CardBody,
} from "reactstrap";
import Sidebar from "../layout/Sidebar";
import Navbar from "../layout/Navbar";
import { FaUserPlus } from "react-icons/fa"; // Import at the top

import {
  isTokenExpired,
  fetchUsers,
  deleteUser,
} from "../../services/userService";
import { useNavigate } from "react-router-dom";
import { useAuth } from "../../context/AuthContext";
import RoleManagement from "../auth/RoleManagement";
import UserManagement from "../auth/UserManagement";

const AdminDashboard = () => {
  const { token, logout } = useAuth();
  const [users, setUsers] = useState([]);
  const [loading, setLoading] = useState(true);
  const [activeTab, setActiveTab] = useState("1");
  const navigate = useNavigate();

  useEffect(() => {
    if (!token || isTokenExpired(token)) {
      logout();
      navigate("/login");
    } else {
      fetchUsers(setUsers, token);
      setLoading(false);
    }

    const intervalId = setInterval(() => {
      if (!token || isTokenExpired(token)) {
        logout();
        navigate("/login");
      }
    }, 20000);

    return () => clearInterval(intervalId);
  }, [token, logout, navigate]);

  const deleteHandler = async (userId) => {
    await deleteUser(userId, users, setUsers, navigate);
  };

  const toggleTab = (tab) => {
    if (activeTab !== tab) setActiveTab(tab);
  };

  return (
    <div className="d-flex">
      <Sidebar />

      <div className="flex-grow-1">
        <Navbar />

        <Container fluid>
          <Row className="align-items-center mb-1 bg-dark  ">
            <Col xs="12" className="text-center mb-2">
              <h3 className="text-light mb-0 fw-bold fs-2 "> 🚀 Admin Dashboard 🎯</h3>
            </Col>

            {/* <Col xs="12" className="text-start">
              <Button className="rounded-pill"
                color="primary"
                onClick={() => navigate("/home/register")}
              >
                <FaUserPlus className="me-2" />
                Create User
              </Button>
            </Col> */}
            
          </Row>

          <Nav tabs>
            <NavItem>
              <NavLink   onClick={() => navigate("/home/register")}>
              <FaUserPlus className="me-2" />
                Create User
              </NavLink>
            </NavItem>
            <NavItem>
              <NavLink
                className={activeTab === "1" ? "active" : ""}
                onClick={() => toggleTab("1")}
              >
                User Management
              </NavLink>
            </NavItem>

            
            <NavItem>
              <NavLink
                className={activeTab === "2" ? "active" : ""}
                onClick={() => toggleTab("2")}
              >
                Role Management
              </NavLink>
             
            </NavItem>
          </Nav>

          <TabContent activeTab={activeTab}>
            <TabPane tabId="1">
              <Card>
                <CardBody>
                  <UserManagement users={users} deleteHandler={deleteHandler} />
                </CardBody>
              </Card>
            </TabPane>

            <TabPane tabId="2">
              <Card>
                <CardBody>
                  <RoleManagement />
                </CardBody>
              </Card>
            </TabPane>
          </TabContent>
        </Container>
      </div>
    </div>
  );
};

export default AdminDashboard;
