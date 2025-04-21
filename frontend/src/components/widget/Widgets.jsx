import React from 'react';
import { FaUsers, FaShoppingCart, FaDollarSign, FaComments } from 'react-icons/fa';
// import './widgets.scss'; // optional: you can style it more

const Widgets = () => {
  const widgetData = [
    {
      icon: <FaUsers className="text-primary fs-2" />,
      title: 'Total Users',
      value: '1,245',
      bg: 'bg-light',
    },
    {
      icon: <FaShoppingCart className="text-success fs-2" />,
      title: 'Orders',
      value: '320',
      bg: 'bg-light',
    },
    {
      icon: <FaDollarSign className="text-warning fs-2" />,
      title: 'Revenue',
      value: '$15,430',
      bg: 'bg-light',
    },
    {
      icon: <FaComments className="text-danger fs-2" />,
      title: 'Feedback',
      value: '87',
      bg: 'bg-light',
    },
  ];

  return (
    <div className="container my-4">
      <div className="row g-4">
        {widgetData.map((item, idx) => (
          <div key={idx} className="col-sm-6 col-lg-3">
            <div className={`card shadow-sm p-3 rounded-4 ${item.bg}`}>
              <div className="d-flex align-items-center gap-3">
                {item.icon}
                <div>
                  <h6 className="mb-0 text-muted">{item.title}</h6>
                  <h4 className="mb-0 fw-bold">{item.value}</h4>
                </div>
              </div>
            </div>
          </div>
        ))}
      </div>
    </div>
  );
};

export default Widgets;
