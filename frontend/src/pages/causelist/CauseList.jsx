import { useState, useEffect } from "react";
import { Table, Container, Card, CardHeader } from 'reactstrap';

import Sidebar from '../../components/layout/Sidebar';
import CustomNavbar from '../../components/layout/Navbar';
import { useAuth } from '../../context/AuthContext';
import { getCauseListData } from "../../services/causeListService";

import { data, useParams } from "react-router-dom";
const CauseList = () => {
    const { listTypeId } = useParams();
    // const cases = [
    //     {
    //         id: 1,
    //         caseDetail: "Case No. 1234/2025",
    //         partyName: "John Doe vs. Jane Doe",
    //         petitionerCounsel: "Adv. Rajesh Mehta",
    //         respondentCounsel: "Adv. Priya Sharma",
    //         caseStatus: "Pending",
    //     },
    //     {
    //         id: 2,
    //         caseDetail: "Case No. 5678/2025",
    //         partyName: "XYZ Corp vs. ABC Ltd",
    //         petitionerCounsel: "Adv. Anil Kapoor",
    //         respondentCounsel: "Adv. Neha Singh",
    //         caseStatus: "Disposed",
    //     },
    //     // Add more entries as needed
    // ];
    const seenSerials = new Set();
    const { token } = useAuth();

    const [causeList, setCauseList] = useState([]);

    useEffect(() => {
        if (token, listTypeId) {
            console.log(token);
            getCauseListData(token, listTypeId)
                .then((res) => setCauseList(res))
                .catch((error) => console.log(error))
        }
    }, [listTypeId]);

    useEffect(() => {
        console.log(causeList)
    }, [])   
    
    const groupedData = causeList.reduce((acc, item) => {
    const key = item.cl_serial_no;
    if (!acc[key]) {
      acc[key] = [];
    }
    acc[key].push(item);
    return acc;
  }, {});
  
    return (
        <div className="d-flex">
            <Sidebar />
            <div className="flex-grow-1">
                <CustomNavbar />


                <Container className="mt-3">
                    <Card>
                        {/* <h6 className="p-2 bg-dark text-light text-uppercase">Fresh Case</h6> */}
                        <CardHeader className="d-flex justify-content-between align-items-center bg-dark text-uppercase">
                            <h5 className="mb-0 text-light">{causeList[0]?.clType?.clt_description}</h5>
                        </CardHeader>
                        <Table bordered hover responsive style={{width:'100%', textAlign: 'center' }}>
                            <thead >
                                <tr>
                                    <th>Sr. No</th>
                                    <th>Case Detail</th>
                                    <th>Party Name</th>
                                    <th>Petitioner's Counsel</th>
                                    <th>Respondent's Counsel</th>
                                    <th>Case Status</th>
                                </tr>
                            </thead>
                            <tbody>
                                {/* {causeList.map((causeList, index) => ( */}
                                {Object.entries(groupedData).map(([serialNo, cases]) => 
                                cases.map((causeList, index) => ((
                                    <tr key={causeList.cl_id}>
                                        {/* <td>{index + 1}</td> */}
                                        <td>{index === 0 ? serialNo : 'with'}</td>
                                        {/* <td onClick={() => handleClick(causeList)}>{causeList.caseType.label}<br/>{causeList.cl_case_no}/{causeList.cl_case_year}</td>  */}
                                        <td>
                                            <a
                                                href={`/case/${causeList.cl_fd_mid}`}
                                                style={{ textDecoration: 'none', color: 'inherit' }}
                                            >
                                                {causeList.caseType.label}
                                                <br />
                                                {causeList.cl_case_no}/{causeList.cl_case_year}
                                            </a>
                                        </td>
                                        <td>{causeList.cl_first_petitioner}<br /><span>vs</span><br />{causeList.cl_first_respondent}</td>

                                        <td>{causeList.cl_petitioner_council}</td>

                                        <td>{causeList.cl_respondent_council}</td>
                                        <td><a href={`http://192.168.0.97/status/view-case-detail/${causeList.cl_ccms_id}`}>Status</a></td>                                       

                                        {/* <td>{causeList.cl_rec_status}</td> */}
                                    </tr>
                                     ))

                                ))}
                            </tbody>
                        </Table>
                    </Card>
                </Container>
            </div>
        </div>
    );
};

export default CauseList