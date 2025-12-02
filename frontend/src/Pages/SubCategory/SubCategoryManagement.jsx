import React, { useEffect, useState } from "react";
import {
  Box,
  Paper,
  Typography,
  TextField,
  Button,
  Table,
  TableHead,
  TableRow,
  TableCell,
  TableBody,
  IconButton,
  FormControl,
  Select,
  MenuItem,
  InputLabel,
} from "@mui/material";
import { Delete } from "@mui/icons-material";
import axios from "axios";
import Sidebar from "../../component/Sidebar";
import Navbar from "../../component/Topbar";

const SubCategoryManagement = () => {
  const [subCategories, setSubCategories] = useState([]);
  const [categories, setCategories] = useState([]);

  const [form, setForm] = useState({
    name: "",
    description: "",
    categoryId: ""
  });

  const BASE_URL = "http://localhost:8080/api/subcategories";
  const CATEGORY_URL = "http://localhost:8080/api/categories";

  useEffect(() => {
    loadCategories();
    loadSubCategories();
  }, []);

  // Fetch categories
  const loadCategories = () => {
    axios.get(CATEGORY_URL)
      .then(res => setCategories(res.data))
      .catch(() => alert("Error fetching categories"));
  };

  // Fetch subcategories
  const loadSubCategories = () => {
    axios.get(BASE_URL)
      .then(res => setSubCategories(res.data))
      .catch(() => alert("Error fetching subcategories"));
  };

  // Add subcategory
  const handleAdd = (e) => {
    e.preventDefault();
    axios.post(BASE_URL, form)
      .then(() => {
        alert("SubCategory added successfully");
        setForm({ name: "", description: "", categoryId: "" });
        loadSubCategories();
      })
      .catch(() => alert("Error adding subcategory"));
  };

  // Delete SubCategory
  const handleDelete = (id) => {
    if (window.confirm("Delete this SubCategory?")) {
      axios.delete(`${BASE_URL}/${id}`)
        .then(() => loadSubCategories())
        .catch(() => alert("Error deleting"));
    }
  };

  return (
    <div style={{ display: "flex" }}>
      <Sidebar />
      <div style={{ flexGrow: 1 }}>
        <Navbar />

        <Box sx={{ p: 4 }}>
          <Typography variant="h5" sx={{ mb: 3, fontWeight: 600 }}>
            SubCategory Management
          </Typography>

          {/* Add SubCategory */}
          <Paper sx={{ p: 3, mb: 3 }}>
            <Typography variant="h6" sx={{ mb: 2 }}>Add SubCategory</Typography>
            <form onSubmit={handleAdd}>
              <TextField
                label="SubCategory Name"
                fullWidth
                required
                sx={{ mb: 2 }}
                value={form.name}
                onChange={(e) => setForm({ ...form, name: e.target.value })}
              />

              <TextField
                label="Description"
                fullWidth
                multiline
                rows={3}
                sx={{ mb: 2 }}
                value={form.description}
                onChange={(e) => setForm({ ...form, description: e.target.value })}
              />

              {/* Category Dropdown */}
              <FormControl fullWidth sx={{ mb: 2 }}>
                <InputLabel>Select Category</InputLabel>
                <Select
                  value={form.categoryId}
                  label="Select Category"
                  onChange={(e) => setForm({ ...form, categoryId: e.target.value })}
                  required
                >
                  {categories.map((c) => (
                    <MenuItem value={c.id} key={c.id}>
                      {c.name}
                    </MenuItem>
                  ))}
                </Select>
              </FormControl>

              <Button variant="contained" type="submit" fullWidth>
                Add SubCategory
              </Button>
            </form>
          </Paper>

          {/* SubCategory List */}
          <Paper sx={{ p: 2 }}>
            <Typography variant="h6" sx={{ mb: 2 }}>SubCategory List</Typography>
            <Table>
              <TableHead>
                <TableRow>
                  <TableCell><strong>Name</strong></TableCell>
                  <TableCell><strong>Description</strong></TableCell>
                  <TableCell><strong>Category</strong></TableCell>
                  <TableCell><strong>Action</strong></TableCell>
                </TableRow>
              </TableHead>
              <TableBody>
                {subCategories.map((s) => (
                  <TableRow key={s.id}>
                    <TableCell>{s.name}</TableCell>
                    <TableCell>{s.description}</TableCell>
                    <TableCell>{s.category?.name}</TableCell>
                    <TableCell>
                      <IconButton onClick={() => handleDelete(s.id)} color="error">
                        <Delete />
                      </IconButton>
                    </TableCell>
                  </TableRow>
                ))}

                {subCategories.length === 0 && (
                  <TableRow>
                    <TableCell colSpan={4} align="center">
                      No SubCategory Available
                    </TableCell>
                  </TableRow>
                )}
              </TableBody>
            </Table>
          </Paper>
        </Box>
      </div>
    </div>
  );
};

export default SubCategoryManagement;
