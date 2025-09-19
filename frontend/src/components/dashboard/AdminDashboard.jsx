import React, { useState } from "react";
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
// import { FaUserPlus } from "react-icons/fa"; // Import at the top
// import { Typewriter } from "react-simple-typewriter";

// import {
//   isTokenExpired,
//   fetchUsers,
//   deleteUser,
// } from "../../services/userService";
// import { useNavigate } from "react-router-dom";
// import { useAuth } from "../../context/AuthContext";
import RoleManagement from "../auth/RoleManagement";
import UserManagement from "../auth/UserManagement";
import "../../assets/styles.css";
import WidgetsDropdown from "../../widget/WidgetsDropdown";
import Register from "../auth/Register";
import UriManagement from "../../components/auth/UriManagement"
const AdminDashboard = () => {
//    Fullstack Java Developer Vijay Chaurasiya

  // const { token, logout } = useAuth();
  // const [users, setUsers] = useState([]);
  // const [loading, setLoading] = useState(true);
  const [activeTab, setActiveTab] = useState("1");
  // const navigate = useNavigate();

  const [rolesFromChild, setRolesFromChild] = useState([]);

  // Callback function to receive roles from the child
  const handleRolesUpdate = (roles) => {
    setRolesFromChild(roles);
    // console.log(rolesFromChild)
  };

  // useEffect(() => {
  //   if (!token || isTokenExpired(token)) {
  //     logout();
  //     navigate("/login");
  //   } else {
  //     fetchUsers(setUsers, token);
  //     setLoading(false);
  //   }

  // const intervalId = setInterval(() => {
  //   if (!token || isTokenExpired(token)) {
  //     logout();
  //     navigate("/login");
  //   }
  // }, 20000);

  //   return () => clearInterval(intervalId);
  // }, [token, logout, navigate]);

  // const deleteHandler = async (userId) => {
  //   await deleteUser(userId, users, setUsers, navigate);
  // };

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
              {/* <h3 className="fs-2 fw-bold text-center text-light my-2">
                {" "}
                <span>
                  <Typewriter
                    words={["Welcome to Admin Dashboard"]}
                    loop={0}
                    cursor
                    cursorStyle="|"
                    typeSpeed={60}
                    deleteSpeed={90}
                    delaySpeed={1500}
                  />
                </span>{" "}
         
              </h3> */}
            </Col>
          </Row>
          {/* <Row className="mb-4">
            <Col xs="12">
              <div className="shadow rounded p-3 bg-white">
                <WidgetsDropdown />
              </div>
            </Col>
          </Row> */}

          <Nav tabs>
            {/* <NavItem>
              <NavLink
                className={activeTab === "0" ? "active" : ""}
                onClick={() => toggleTab("0")}
              >
                Create User
              </NavLink>
            </NavItem> */}
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

             <NavItem>
              <NavLink
                className={activeTab === "3" ? "active" : ""}
                onClick={() => toggleTab("3")}
              >
               Uri Management
              </NavLink>
            </NavItem>
          </Nav>

          <TabContent activeTab={activeTab}>
            <TabPane tabId="1">
              <Card>
                <CardBody>
                  <UserManagement />
                </CardBody>
              </Card>
            </TabPane>

            <TabPane tabId="2">
              <Card>
                <CardBody>
                  <RoleManagement onRolesUpdate={handleRolesUpdate} />{" "}
                  {/* Pass the callback */}
                </CardBody>
              </Card>
            </TabPane>
            <TabPane tabId="3">
              <Card>
                <CardBody>              
                    <UriManagement setActiveTab={setActiveTab} />
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
