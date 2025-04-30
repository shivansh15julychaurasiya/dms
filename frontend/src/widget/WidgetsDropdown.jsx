import React from 'react';
import {
  CRow,
  CCol,
  CWidgetStatsA,
} from '@coreui/react';
import CIcon from '@coreui/icons-react';
import {
  cibFacebook,
  cibTwitter,
  cibLinkedin,
  cibInstagram,
} from '@coreui/icons';

const WidgetsDropdown = () => {
  return (
    <CRow className="g-4">
      <CCol xs={12} sm={6} lg={3}>
        <CWidgetStatsA
          className="h-100"
          color="primary"
          value="Facebook"
          title="Digital Notice Board"
          action={<CIcon icon={cibFacebook} height={110} className="text-white" />}
        />
      </CCol>
      <CCol xs={12} sm={6} lg={3}>
        <CWidgetStatsA
          className="h-100"
          color="info"
          value="Notice"
          title="Display Board"
          action={<CIcon icon={cibTwitter} height={110} className="text-white" />}
        />
      </CCol>
      <CCol xs={12} sm={6} lg={3}>
        <CWidgetStatsA
          className="h-100"
          color="warning"
          value="Cause List"
          title="Live Update"
          action={<CIcon icon={cibLinkedin} height={110} className="text-white" />}
        />
      </CCol>
      <CCol xs={12} sm={6} lg={3}>
        <CWidgetStatsA
          className="h-100"
          color="danger"
          value="Court Room"
          title="Status"
          action={<CIcon icon={cibInstagram} height={110} className="text-white" />}
        />
      </CCol>
    </CRow>
  );
};

export default WidgetsDropdown;
