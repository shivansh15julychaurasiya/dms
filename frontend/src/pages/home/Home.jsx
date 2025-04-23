import React from "react";
import "./home.scss";


import Sidebar from "../../components/sidebar/Sidebar";
import Navbar from "../../components/navbar/Navbar";
import Widgets from "../../components/widget/Widgets";
import TableData from "../../components/table/TableData";

export default function Home() {
  return (
    <div className="home">
      <Sidebar />
      <div className="homeContainer">
        <Navbar />
        <Widgets className="mt-5" />

       
        <div className="listContainer">
          <div className="listTitle">Latest Transaction</div>
          <TableData />
          {/* <DashboardLayout/> */}
        </div>
      </div>
    </div>
  );
}
