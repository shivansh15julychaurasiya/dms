import React, { useEffect } from 'react';
import { Container, Row, Col, Card, CardBody, CardTitle, Button } from 'reactstrap';
import ".././assets/styles.css"
const LandingPage = () => {
  // Prevent body scroll when LandingPage is active
  useEffect(() => {
    document.body.style.overflow = 'hidden';

    return () => {
      document.body.style.overflow = 'auto'; // reset on unmount
    };
  }, []);

  return (
    <div
      style={{
        height: '100vh',
        backgroundImage: 'url("https://as2.ftcdn.net/jpg/06/12/69/39/1000_F_612693965_Ic0XfvkMa44xQXHA8lonULgqoEzyS0Xl.jpg")',
        backgroundSize: 'cover',
        backgroundPosition: 'center',
        backgroundAttachment: 'fixed',
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
      }}
    >
      <Container className="text-white text-center">
        <Row>
          <Col md="10" className="mx-auto">
            <Card className="bg-transparent border-0 p-5 text-white">
              <CardBody>
                <CardTitle tag="h1" className="display- shimmer-text">
                 WELCOME TO ALLAHABAD HIGH COURT
                </CardTitle>
                <p className="lead">
                  Explore our legal resources, connect with experts, and manage your case smartly.
                </p>
                <Button color="light" size="lg" href="/signup">
                  Get Started
                </Button>
              </CardBody>
            </Card>
          </Col>
        </Row>
      </Container>
    </div>
  );
};

export default LandingPage;
