import React from "react";
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

const caseTypes = [
  { title: "Fresh Cases (Sl.No. upto 1000)", total: 0 },
  { title: "Daily Cause List", total: 0 },
  { title: "Daily IA (SL. No. 8001 onwards)", total: 0 },
  { title: "Correction Application", total: 0 },
  { title: "'As-Fresh' List (Sl.No. 1001 to 3000)", total: 0 },
  { title: "Backlog", total: 0 },
  { title: "Fresh Supplementry", total: 0 },
  { title: "Additional/Unlisted (Sl. No. 3001 To 8000)", total: 0 },
];

const EcourtDashboard = () => {
  return (
    <div className="d-flex">
      <Sidebar />
      <div className="flex-grow-1">
        <CustomNavbar />

        <Container className="py-4">
          <Row className="g-4">
            {caseTypes.map((item, index) => (
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
                        {item.title}
                      </CardTitle>
                      <CardText className="mt-2">
                        Total Case Listed: <strong>{item.total}</strong>
                      </CardText>
                    </div>
                    <Button color="light" outline className="mt-3 w-100">
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
