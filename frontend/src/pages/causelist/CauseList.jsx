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

    const { token } = useAuth();

    const [causeList, setCauseList] = useState([]);

    useEffect(() => {
        if (token, listTypeId) {
            console.log(token);
            getCauseListData(token, listTypeId)
                .then((res) => setCauseList(res))
                .catch((error) => console.log(error))
        }
    }, []);
    useEffect(() => {
        console.log(causeList)
    }, [])

    return (
        <div className="d-flex">
            <Sidebar />
            <div className="flex-grow-1">
                <CustomNavbar />


                <Container className="mt-3">
                    <Card>
                        {/* <h6 className="p-2 bg-dark text-light text-uppercase">Fresh Case</h6> */}
                        <CardHeader className="d-flex justify-content-between align-items-center bg-dark text-uppercase">
                            <h5 className="mb-0 text-light">Fresh Case</h5>
                        </CardHeader>
                        <Table bordered hover responsive style={{ display: 'block', textAlign: 'center' }}>
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
                                {causeList.map((causeList, index) => (
                                    <tr key={causeList.cl_id}>
                                        <td>{index + 1}</td>
                                        {/* <td onClick={() => handleClick(causeList)}>{causeList.caseType.label}<br/>{causeList.cl_case_no}/{causeList.cl_case_year}</td>  */}
                                        <td>
                                            <a
                                                href={`/case/${causeList.cl_case_no}/${causeList.cl_case_year}`}
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

                                        <td>{causeList.cl_rec_status}</td>
                                    </tr>
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