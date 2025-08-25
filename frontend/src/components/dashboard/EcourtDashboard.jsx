import React, { useEffect, useState } from 'react'
import {
  Card,
  CardBody,
  CardTitle,
  CardText,
  Button,
  Row,
  Col,
  Container,
} from "reactstrap";
import Sidebar from "../layout/Sidebar";
import CustomNavbar from "../layout/Navbar";
import { getCauseListTotal } from "../../services/causeListService";
import { useAuth } from '../../context/AuthContext';
import { data } from 'react-router-dom';

// const caseTypes = [
//   { title: "Fresh Cases (Sl.No. upto 1000)", total: 0 },
//   { title: "Daily Cause List", total: 0 },
//   { title: "Daily IA (SL. No. 8001 onwards)", total: 0 },
//   { title: "Correction Application", total: 0 },
//   { title: "'As-Fresh' List (Sl.No. 1001 to 3000)", total: 0 },
//   { title: "Backlog", total: 0 },
//   { title: "Fresh Supplementry", total: 0 },
//   { title: "Additional/Unlisted (Sl. No. 3001 To 8000)", total: 0 },
// ];
 
 //let listTypeId = 5;


const EcourtDashboard = () => {

  const { token } = useAuth();
  
      const [causeListCount, setCauseListCount] = useState([]);
  
      useEffect(() => {
          if (token) {
              getCauseListTotal(token)
                  .then((res) => setCauseListCount(res))
                  .catch((error) => console.log(error))
          }
      }, [])
      useEffect(() => {
              console.log(causeListCount)
      })
  
      // const handleChange = (e) => {
      //     setSelectedOption(e.target.value);
      // };
  return (
    <div className="d-flex">
      <Sidebar />
      <div className="flex-grow-1">
        <CustomNavbar />

        <Container className="py-4">
          <Row className="g-4">
            
            {causeListCount.map((item, index) => (              
              <Col
                key={index}
                xs="12"
                sm="6"
                md="4"
                lg="3"
                className="d-flex align-items-stretch"
              >
                
                <Card
                  className="shadow-sm w-100 border-0"
                  style={{
                    background: "linear-gradient(135deg, #0d6efd, #00bcd4)",
                    color: "#fff",
                    borderRadius: "1rem",
                  }}
                >
                 
                  <CardBody className="d-flex flex-column justify-content-between">
                    <div>
                      <CardTitle tag="h5" className="fw-bold">
                        {item.clTypeData} & {item.cl_list_type_mid}
                      </CardTitle>
                      <CardText className="mt-2">
                        Total Case Listed: <strong>{item.count}</strong>
                      </CardText>
                    </div>
                    <Button color="light" outline className="mt-3 w-100"
                      //  onClick={() => window.open("/dms/causelist", "_blank")}>
                        // {/* onClick={() => window.open(`/dms/causelist/${item.cl_list_type_mid}`, "_blank")}> */}
                       onClick={() => window.open(`/dms/cause-lists/getCauseList/${item.cl_list_type_mid}`, "_blank")}> 
                      View Detail
                    </Button>
                  </CardBody>
                </Card>                
              </Col>
            ))}
          </Row>

          <div
            className="mt-5 text-center text-danger fw-semibold"
            style={{ fontSize: "0.95rem" }}
          >
            {/* IS data. Any changes/ modification in case listing data in CIS, the
            same will be reflected in e-Court also. */}
          </div>
        </Container>
      </div>
    </div>
  );
};

export default EcourtDashboard;
