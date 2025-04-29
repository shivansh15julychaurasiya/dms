import React, { useEffect, useState } from "react";
import { Button, Container, Row, Col, Nav, NavItem, NavLink, TabContent, TabPane, Card, CardBody } from "reactstrap";
import Sidebar from "../layout/Sidebar";
import Navbar from "../layout/Navbar";
import { isTokenExpired, fetchUsers, deleteUser } from "../../services/userService";
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
          <Row className="align-items-center">
            <Col xs="12" md="6">
              <h4 className="text-primary">Admin Dashboard</h4>
            </Col>
            <Col xs="12" md="6" className="text-md-end mt-2 mt-md-0">
              <Button color="primary" onClick={() => navigate("/home/register")}>
                Create User
              </Button>
            </Col>
          </Row>

          <Nav tabs>
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
