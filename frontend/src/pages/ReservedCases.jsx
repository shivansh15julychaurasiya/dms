import React from "react";
import { Card, CardBody, CardHeader, Table } from "reactstrap";
import Sidebar from "../components/layout/Sidebar";
import Navbar from "../../src/components/layout/Navbar"


const ReservedCases = () => {
  const reservedCases = [
    {
      id: 1,
      caseNumber: "HC/2024/001",
      title: "State vs. John Doe",
      judge: "Justice Sharma",
      reservedDate: "2024-12-15",
      status: "Reserved for Judgment",
    },
    {
      id: 2,
      caseNumber: "HC/2024/002",
      title: "XYZ Corp vs. ABC Ltd",
      judge: "Justice Verma",
      reservedDate: "2024-12-18",
      status: "Pending Review",
    },
    {
      id: 3,
      caseNumber: "HC/2024/003",
      title: "Rajiv Singh vs. State",
      judge: "Justice Kapoor",
      reservedDate: "2024-12-20",
      status: "Reserved",
    },
  ];

  return (
   <div className="d-flex">
    <Sidebar/>
    <div className="flex-grow-1">
        <Navbar/>
        
        <Card className="mt-3 shadow-sm">
      <CardHeader tag="h5" className="bg-primary text-white text-center">
        Reserved Cases
      </CardHeader>
      <CardBody>
        <Table responsive bordered hover>
          <thead className="table-dark">
            <tr>
              <th>#</th>
              <th>Case No</th>
              <th>Title</th>
              <th>Judge</th>
              <th>Date Reserved</th>
              <th>Status</th>
            </tr>
          </thead>
          <tbody>
            {reservedCases.map((item, index) => (
              <tr key={item.id}>
                <td>{index + 1}</td>
                <td>{item.caseNumber}</td>
                <td>{item.title}</td>
                <td>{item.judge}</td>
                <td>{item.reservedDate}</td>
                <td>{item.status}</td>
              </tr>
            ))}
          </tbody>
        </Table>
      </CardBody>
    </Card>
    </div>
   </div>
  );
};

export default ReservedCases;
