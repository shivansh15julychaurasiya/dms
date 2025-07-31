import React, { useEffect, useState } from 'react'
import Sidebar from '../../components/layout/Sidebar'
import Navbar from '../../components/layout/Navbar'
import {
    Container,
    Row,
    Col,
    Table,
    Input,
    Button,
    Modal,
    ModalHeader,
    ModalBody,
    FormGroup,
    Label,
    Card,
    CardHeader,
    ModalFooter,
    CardBody,

} from "reactstrap";
import { fetchCourtMaster } from '../../services/courtManage';
import { useAuth } from '../../context/AuthContext';




// function ManageBench() {
const ManageBench = () => {

    const [modal, setModal] = useState(false);
    const [unmountOnClose, setUnmountOnClose] = useState(true);
    const [selectedOption, setSelectedOption] = useState('option1');

    const toggle = () => setModal(!modal);
    const changeUnmountOnClose = (e) => {
        let { value } = e.target;
        setUnmountOnClose(JSON.parse(value));
    };

    const { token } = useAuth();

    const [courtMaster, setCourtMaster] = useState([]);

    useEffect(() => {
        if (token) {
            fetchCourtMaster(token)
                .then((res) => setCourtMaster(res.data))
                .catch((error) => console.log(error))
        }
    }, [])
    useEffect(() => {
        //    console.log(courtMaster)
    })

    const handleChange = (e) => {
        setSelectedOption(e.target.value);
    };

    return (
        <div className="d-flex">
            <Sidebar />
            <div className="flex-grow-1">
                <Navbar />
                <Container fluid className="mt-3">
                    <Row>
                        <Col xs="12">
                            <Card inverse>
                                <CardHeader className="d-flex justify-content-between align-items-center">
                                    <h5 className="mb-0 text-dark">Manage Benches</h5>
                                    <Button color="light" size="sm">
                                        <i className="fa fa-expand" />
                                    </Button>
                                </CardHeader>
                                <CardBody>
                                    <div className='d-flex justify-content-md-end'>
                                        <td>
                                            <Button color="danger" onClick={toggle}>
                                                Add New Court
                                            </Button>
                                        </td>
                                    </div>
                                    <Table bordered responsive>
                                        <thead>
                                            <tr>
                                                <th style={{ textAlign: "center", width: "25%" }}>
                                                    Court Name
                                                </th>
                                                <th style={{ textAlign: "center", width: "15%" }}>
                                                    Bench Id
                                                </th>
                                                <th style={{ textAlign: "center", width: "15%" }}>
                                                    Action
                                                </th>
                                                <th style={{ textAlign: "center", width: "45%" }}>
                                                    Service
                                                </th>
                                            </tr>
                                        </thead>
                                        <tbody style={{ textAlign: "center" }}>
                                            {courtMaster.map((court, index) => (
                                                <tr key={court.cmId || index}>
                                                    <td>{court.cmName}</td>
                                                    <td>{court.cmBenchId}</td>
                                                    <td>
                                                        <button type="button" class="btn btn-primary">Edit</button>
                                                    </td>
                                                    <td>
                                                        <button type="button" class="btn btn-primary">Supplementary</button>
                                                        <button type="button" class="btn btn-secondary">Transfer</button>
                                                        <button type="button" class="btn btn-success">Correction&Mention</button>
                                                    </td>
                                                </tr>
                                            ))}
                                        </tbody>
                                    </Table>
                                </CardBody>
                            </Card>
                        </Col>
                    </Row>
                </Container>

                <div>
                    <Modal isOpen={modal} toggle={toggle} unmountOnClose={unmountOnClose}>
                        <ModalHeader toggle={toggle}>Add New Court</ModalHeader>
                        <ModalBody>
                            <label htmlFor="" className='mb-3'>Court Name</label>
                            <Input
                                type="text"
                                placeholder="Please Enter Court"
                            />
                            <label htmlFor="" className='mt-3 mb-2'>Status</label>
                            <div className='d-flex'>
                                <FormGroup className='d-inline-block me-3'>
                                    <Input
                                        type="radio"
                                        name="radioOption"
                                        value="option1"
                                        checked={selectedOption === 'option1'}
                                        onChange={handleChange}
                                    />
                                    <Label check className='ms-2'>

                                        E-Court
                                    </Label>

                                </FormGroup>
                                <FormGroup className='d-inline-block '>
                                    <Input
                                        type="radio"
                                        name="radioOption"
                                        value="option2"
                                        checked={selectedOption === 'option2'}
                                        onChange={handleChange}
                                    />
                                    <Label check className='ms-2'>

                                        In-Chamber
                                    </Label>

                                </FormGroup>
                            </div>
                        </ModalBody>
                        <ModalFooter>
                            <Button color="primary" onClick={toggle}>
                                Save
                            </Button>{' '}
                            <Button color="secondary" onClick={toggle}>
                                Cancel
                            </Button>
                        </ModalFooter>
                    </Modal>
                </div>
            </div>
        </div>
    )
}

export default ManageBench