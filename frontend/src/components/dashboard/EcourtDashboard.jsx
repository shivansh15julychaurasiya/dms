import React from 'react'
import Sidebar from '../layout/Sidebar'
import CustomNavbar from '../layout/Navbar'
import WidgetsDropdown from '../../widget/WidgetsDropdown'

import {
    Container,
    Row,
    Col,
    Card,
    CardHeader,
    CardBody,
    Table,
    Button,
    Input,
    FormGroup,

} from 'reactstrap';

import { CRow, CWidgetStatsA, CCol, } from '@coreui/react';
import { CIcon } from '@coreui/icons-react';
// import { cilUser } from '@coreui/icons'; // Optional: an icon


function EcourtDashboard() {
    return (
        <div className="d-flex">
            <Sidebar/>
            <div className="flex-grow-1">
                <CustomNavbar />

                <Container fluid className="mt-5">
                    <CRow className="g-4">
                        <CCol xs={12} sm={6} lg={3}>
                            <CWidgetStatsA
                                 className="h-100 text-white"
                                //   color="primary"
                                style={{
                                    background: 'linear-gradient(135deg, #6a11cb 0%, #2575fc 100%)',
                                    borderRadius: '12px',
                                    padding: '20px',
                                }}
                                value="Users"
                                title="Digital Notice Board"
                                action={<CIcon height={110} className="text-white" />}
                            />
                        </CCol>
                        <CCol xs={12} sm={6} lg={3}>
                            <CWidgetStatsA
                                 className="h-100 text-white"
                                style={{
                                    background: 'linear-gradient(135deg, #6a11cb 0%, #2575fc 100%)',
                                    borderRadius: '12px',
                                    padding: '20px',
                                }}
                                value="Role"
                                title="Display Board"
                                action={<CIcon height={110} className="text-white" />}
                            />
                        </CCol>
                        <CCol xs={12} sm={6} lg={3}>
                            <CWidgetStatsA
                                className="h-100 text-white"
                                style={{
                                    background: 'linear-gradient(135deg, #6a11cb 0%, #2575fc 100%)',
                                    borderRadius: '12px',
                                    padding: '20px',
                                }}
                                value="Cause List"
                                title="Live Update"
                                action={<CIcon height={110} className="text-white" />}
                            />
                        </CCol>
                        <CCol xs={12} sm={6} lg={3}>
                            <CWidgetStatsA
                                className="h-100 text-white"
                                style={{
                                    background: 'linear-gradient(135deg, #6a11cb 0%, #2575fc 100%)',
                                    borderRadius: '12px',
                                    padding: '20px',
                                }}
                                value="Court Room"
                                title="Status"
                                action={<CIcon height={110} className="text-white" />}
                            />
                        </CCol>
                    </CRow>

                    <CRow className="g-4 mt-2">
                        <CCol xs={12} sm={6} lg={3}>
                            <CWidgetStatsA
                                 className="h-100 text-white"
                                style={{
                                    background: 'linear-gradient(135deg, #6a11cb 0%, #2575fc 100%)',
                                    borderRadius: '12px',
                                    padding: '20px',
                                }}
                                value="Users"
                                title="Digital Notice Board"
                                action={<CIcon height={110} className="text-white" />}
                            />
                        </CCol>
                        <CCol xs={12} sm={6} lg={3}>
                            <CWidgetStatsA
                                className="h-100 text-white"
                                style={{
                                    background: 'linear-gradient(135deg, #6a11cb 0%, #2575fc 100%)',
                                    borderRadius: '12px',
                                    padding: '20px',
                                }}
                                value="Role"
                                title="Display Board"
                                action={<CIcon height={110} className="text-white" />}
                            />
                        </CCol>
                        <CCol xs={12} sm={6} lg={3}>
                            <CWidgetStatsA
                                 className="h-100 text-white"
                                style={{
                                    background: 'linear-gradient(135deg, #6a11cb 0%, #2575fc 100%)',
                                    borderRadius: '12px',
                                    padding: '20px',
                                }}
                                value="Cause List"
                                title="Live Update"
                                action={<CIcon height={110} className="text-white" />}
                            />
                        </CCol>
                        <CCol xs={12} sm={6} lg={3}>
                            <CWidgetStatsA
                                className="h-100 text-white"
                                style={{
                                    background: 'linear-gradient(135deg, #6a11cb 0%, #2575fc 100%)',
                                    borderRadius: '12px',
                                    padding: '20px',
                                }}
                                value="Court Room"
                                title="Status"
                                action={<CIcon height={110} className="text-white" />}
                            />
                        </CCol>
                    </CRow>
                </Container>



            </div>
        </div>

    )
}

export default EcourtDashboard