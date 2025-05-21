import React, { useState, useEffect } from 'react';
import {
  Container, Row, Col, Table, Button, Form, FormGroup, Label, Input
} from 'reactstrap';

const UrlManagement = () => {
  const [urls, setUrls] = useState(() => {
    const stored = localStorage.getItem('urls');
    return stored ? JSON.parse(stored) : [];
  });

  const [formData, setFormData] = useState({ name: '', url: '' });
  const [editingIndex, setEditingIndex] = useState(null);

  useEffect(() => {
    localStorage.setItem('urls', JSON.stringify(urls));
  }, [urls]);

  const isValidUrl = (url) => {
    try {
      new URL(url);
      return true;
    } catch {
      return false;
    }
  };

  const normalizeUrl = (url) => {
    if (!/^https?:\/\//i.test(url)) {
      return 'https://' + url;
    }
    return url;
  };

  const handleSubmit = (e) => {
    e.preventDefault();
    const { name, url } = formData;

    if (!name.trim() || !url.trim()) return alert('Name and URL are required.');

    const normalizedUrl = normalizeUrl(url);

    if (!isValidUrl(normalizedUrl)) return alert('Invalid URL format.');

    const entry = { name: name.trim(), url: normalizedUrl };

    if (editingIndex !== null) {
      const updated = [...urls];
      updated[editingIndex] = entry;
      setUrls(updated);
      setEditingIndex(null);
    } else {
      setUrls([...urls, entry]);
    }

    setFormData({ name: '', url: '' });
  };

  const handleEdit = (index) => {
    setFormData(urls[index]);
    setEditingIndex(index);
  };

  const handleDelete = (index) => {
    if (window.confirm('Are you sure you want to delete this entry?')) {
      const updated = urls.filter((_, i) => i !== index);
      setUrls(updated);
    }
  };

  return (
    <Container className="mt-5">
      <h2 className="mb-4 text-center">URL Management System</h2>

      <Form onSubmit={handleSubmit}>
        <Row>
          <Col md={5}>
            <FormGroup>
              <Label for="name">Name</Label>
              <Input
                id="name"
                name="name"
                value={formData.name}
                onChange={(e) => setFormData({ ...formData, name: e.target.value })}
                placeholder="Google"
                required
              />
            </FormGroup>
          </Col>
          <Col md={5}>
            <FormGroup>
              <Label for="url">URL</Label>
              <Input
                id="url"
                name="url"
                value={formData.url}
                onChange={(e) => setFormData({ ...formData, url: e.target.value })}
                placeholder="https://www.google.com"
                required
              />
            </FormGroup>
          </Col>
          <Col md={2} className="d-flex align-items-end">
            <Button color="primary" type="submit" block>
              {editingIndex !== null ? 'Update' : 'Add'}
            </Button>
          </Col>
        </Row>
      </Form>

      <hr />

      <Table responsive bordered hover>
        <thead className="thead-dark">
          <tr>
            <th>#</th>
            <th>Name</th>
            <th>URL</th>
            <th>Actions</th>
          </tr>
        </thead>
        <tbody>
          {urls.length === 0 ? (
            <tr>
              <td colSpan="4" className="text-center">No URLs added yet.</td>
            </tr>
          ) : (
            urls.map((item, index) => (
              <tr key={index}>
                <th scope="row">{index + 1}</th>
                <td>{item.name}</td>
                <td><a href={item.url} target="_blank" rel="noopener noreferrer">{item.url}</a></td>
                <td>
                  <Button size="sm" color="info" className="me-2" onClick={() => handleEdit(index)}>Edit</Button>
                  <Button size="sm" color="danger" onClick={() => handleDelete(index)}>Delete</Button>
                </td>
              </tr>
            ))
          )}
        </tbody>
      </Table>
    </Container>
  );
};

export default UrlManagement;
