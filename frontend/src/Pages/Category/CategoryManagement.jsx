import React, { useEffect, useState } from "react";
import {
  Box,
  Button,
  TextField,
  Typography,
  Table,
  TableBody,
  TableCell,
  TableContainer,
  TableHead,
  TableRow,
  Paper,
} from "@mui/material";
import Sidebar from "../../component/Sidebar";
import Navbar from "../../component/Topbar";
import axios from "axios";

const CategoryManagement = () => {
  const [categoryName, setCategoryName] = useState("");
  const [categories, setCategories] = useState([]);
  const [loading, setLoading] = useState(false);

  // API Base URL
  const BASE_URL = "http://localhost:8080/api/category";

  // Load all categories
  const fetchCategories = async () => {
    try {
      const res = await axios.get(`${BASE_URL}/get`);
      setCategories(res.data);
    } catch (error) {
      console.log("Error loading categories:", error);
    }
  };

  useEffect(() => {
    fetchCategories();
  }, []);

  // Add category
  const handleAddCategory = async () => {
    if (categoryName.trim() === "") {
      alert("Category cannot be empty");
      return;
    }
    setLoading(true);
    try {
      await axios.post(`${BASE_URL}/add`, { name: categoryName });
      setCategoryName("");
      fetchCategories();
    } catch (error) {
      console.log("Error adding:", error);
      alert("Category already exists");
    }
    setLoading(false);
  };

  // Delete category
  const handleDelete = async (id) => {
    if (!window.confirm("Are you sure want to delete this category?")) return;

    try {
      await axios.delete(`${BASE_URL}/delete/${id}`);
      fetchCategories();
    } catch (error) {
      console.log("Error deleting:", error);
    }
  };

  return (
    <div style={{ display: "flex" }}>
      {/* Sidebar */}
      <Sidebar />

      {/* Main Content */}
      <div style={{ flexGrow: 1 }}>
        <Navbar />

        <Box sx={{ p: 4 }}>
          <Typography variant="h5" sx={{ mb: 3, fontWeight: 600 }}>
            Category Management
          </Typography>

          {/* ADD CATEGORY FORM */}
          <Box
            sx={{
              display: "flex",
              gap: 2,
              alignItems: "center",
              mb: 4,
              maxWidth: "500px",
            }}
          >
            <TextField
              fullWidth
              label="Category Name"
              value={categoryName}
              onChange={(e) => setCategoryName(e.target.value)}
            />
              <TextField
              fullWidth
              label="Category Description"
              value={categoryName}
              onChange={(e) => setCategoryName(e.target.value)}
            />
            <Button
              variant="contained"
              onClick={handleAddCategory}
              disabled={loading}
            >
              {loading ? "Saving..." : "Add"}
            </Button>
          </Box>

          {/* CATEGORY TABLE */}
          <TableContainer component={Paper}>
            <Table>
              <TableHead sx={{ background: "#eee" }}>
                <TableRow>
                  <TableCell>#</TableCell>
                  <TableCell>Category Name</TableCell>
                  <TableCell>Action</TableCell>
                </TableRow>
              </TableHead>

              <TableBody>
                {categories.map((cat, index) => (
                  <TableRow key={cat.id}>
                    <TableCell>{index + 1}</TableCell>
                    <TableCell>{cat.name}</TableCell>
                    <TableCell>
                      <Button
                        variant="outlined"
                        color="error"
                        onClick={() => handleDelete(cat.id)}
                      >
                        Delete
                      </Button>
                    </TableCell>
                  </TableRow>
                ))}

                {categories.length === 0 && (
                  <TableRow>
                    <TableCell colSpan={3} align="center">
                      No Category Found
                    </TableCell>
                  </TableRow>
                )}
              </TableBody>
            </Table>
          </TableContainer>
        </Box>
      </div>
    </div>
  );
};

export default CategoryManagement;
