import React from "react";
import "bootstrap/dist/css/bootstrap.min.css";
import "bootstrap/dist/js/bootstrap.bundle.min.js";

const FancyAccordion = () => {
  return (
    <div className="container-fluid">
      <div className="accordion shadow rounded" id="customAccordion">

        {/* Accordion Item 1 */}
        <div className="accordion-item border-0 mb-3 rounded shadow-sm">
          <h2 className="accordion-header" id="headingOne">
            <button
              className="accordion-button fw-bold text-primary"
              type="button"
              data-bs-toggle="collapse"
              data-bs-target="#collapseOne"
              aria-expanded="true"
              aria-controls="collapseOne"
            >
              Office report
            </button>
          </h2>
          <div
            id="collapseOne"
            className="accordion-collapse collapse show"
            aria-labelledby="headingOne"
            data-bs-parent="#customAccordion"
          >
            <div className="accordion-body">
              Name: John Doe <br />
              Email: john@example.com <br />
              Role: Admin
            </div>
          </div>
        </div>

        {/* Accordion Item 2 */}
        <div className="accordion-item border-0 mb-3 rounded shadow-sm">
          <h2 className="accordion-header" id="headingTwo">
            <button
              className="accordion-button collapsed fw-bold text-success"
              type="button"
              data-bs-toggle="collapse"
              data-bs-target="#collapseTwo"
              aria-expanded="false"
              aria-controls="collapseTwo"
            >
              Pettion
            </button>
          </h2>
          <div
            id="collapseTwo"
            className="accordion-collapse collapse"
            aria-labelledby="headingTwo"
            data-bs-parent="#customAccordion"
          >
            <div className="accordion-body">
              - Meeting on Tuesday <br />
              - Case Deadline: April 12th <br />
              - Notification Alerts: On
            </div>
          </div>
        </div>

        {/* Accordion Item 3 */}
        <div className="accordion-item border-0 mb-3 rounded shadow-sm">
          <h2 className="accordion-header bg-dark" id="headingThree">
            <button
              className="accordion-button collapsed fw-bold text-danger"
              type="button"
              data-bs-toggle="collapse"
              data-bs-target="#collapseThree"
              aria-expanded="false"
              aria-controls="collapseThree"
            >
              Pettioners
            </button>
          </h2>
          <div
            id="collapseThree"
            className="accordion-collapse collapse"
            aria-labelledby="headingThree"
            data-bs-parent="#customAccordion"
          >
            <div className="accordion-body">
              - Weekly Case Stats <br />
              - Pending Cases: 12 <br />
              - Resolved: 8
            </div>
          </div>
        </div>

      </div>
    </div>
  );
};

export default FancyAccordion;
