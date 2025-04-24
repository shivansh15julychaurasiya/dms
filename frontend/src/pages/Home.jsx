import React from "react";
import "../assets/styles.css"


import Sidebar from "../components/layout/Sidebar";
import Navbar from "../components/layout/Navbar";
import LandingPage from "./LandingPage";

export default function Home() {
  return (
    <div className="home">
      <Sidebar />
      <div className="homeContainer">
        <Navbar />
       <LandingPage/>

       
       
      </div>
    </div>
  );
}
