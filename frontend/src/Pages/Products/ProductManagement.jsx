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
  MenuItem,
  Select,
  InputLabel,
  FormControl,
} from "@mui/material";

import Sidebar from "../../component/Sidebar";
import Navbar from "../../component/Topbar";

import { createProduct } from "../../services/ProductService";
import { getAllCategories } from "../../services/CategoryService";
import { getAllSubCategories } from "../../services/SubCategoryService";
import { useAuth } from "../../context/AuthContext";
const ProductManagement = () => {
  const [name, setName] = useState("");
  const [price, setPrice] = useState("");
  const [stock, setStock] = useState("");
  const [categoryId, setCategoryId] = useState("");
  const [subCategoryId, setSubCategoryId] = useState("");
  const [image, setImage] = useState(null);

  const [categories, setCategories] = useState([]);
  const [subcategories, setSubcategories] = useState([]);
  const [products, setProducts] = useState([]);
 const { token } = useAuth(); 


  // ------------------------------------
  // FETCH CATEGORIES
  // ------------------------------------
  const fetchCategories = async () => {
    try {
      const res = await getAllCategories();
      console.log(res);
      setCategories(res);
    } catch (error) {
      console.log("Error fetching categories", error);
    }
  };

  // ------------------------------------
  // FETCH SUB-CATEGORIES
  // ------------------------------------
  const fetchSubCategories = async () => {
    try {
      const res = await getAllSubCategories(token);
      setSubcategories(res);
    } catch (error) {
      console.log("Error fetching subcategories", error);
    }
  };

useEffect(() => {
  fetchCategories(); // always load categories once
}, []);

useEffect(() => {
  if (categoryId) fetchSubCategoriesByCategory(categoryId);
}, [categoryId]);


  //  Load subcategories only when category changes


  // ------------------------------------
  // ADD PRODUCT
  // ------------------------------------
  const handleAddProduct = async () => {
    if (!name || !price || !stock || !categoryId || !subCategoryId) {
      alert("Fill all fields");
      return;
    }

    const data = { name, price, stock, categoryId, subCategoryId, image };

    try {
      await createProduct(token, data);
      alert("Product added successfully");

      setName("");
      setPrice("");
      setStock("");
      setCategoryId("");
      setSubCategoryId("");
      setImage(null);

      // fetchProducts(); // if you add product listing you can uncomment
    } catch (error) {
      alert("Error while adding product");
      console.log(error);
    }
  };

  return (
    <div style={{ display: "flex" }}>
      <Sidebar />
      <div style={{ flexGrow: 1 }}>
        <Navbar />

        <Box sx={{ p: 4 }}>
          <Typography variant="h5" sx={{ mb: 3, fontWeight: 600 }}>
            Product Management
          </Typography>

          {/* ADD FORM */}
          <Box
            sx={{
              display: "grid",
              gridTemplateColumns: "repeat(2, 1fr)",
              gap: 2,
              mb: 4,
              maxWidth: "650px",
            }}
          >
            <TextField
              label="Product Name"
              value={name}
              onChange={(e) => setName(e.target.value)}
            />

            <TextField
              label="Price"
              type="number"
              value={price}
              onChange={(e) => setPrice(e.target.value)}
            />

            <TextField
              label="Stock"
              type="number"
              value={stock}
              onChange={(e) => setStock(e.target.value)}
            />

            {/* CATEGORY */}
            <FormControl>
              <InputLabel>Select Category</InputLabel>
              <Select
                value={categoryId}
                label="Select Category"
                onChange={(e) => setCategoryId(e.target.value)}
              >
                {categories.map((cat) => (
                  <MenuItem key={cat.id} value={cat.id}>
                    {cat.name}
                  </MenuItem>
                ))}
              </Select>
            </FormControl>

            {/* SUB-CATEGORY */}
            <FormControl>
              <InputLabel>Select SubCategory</InputLabel>
              <Select
                value={subCategoryId}
                label="Select SubCategory"
                onChange={(e) => setSubCategoryId(e.target.value)}
                disabled={!categoryId}
              >
                {subcategories.length > 0 ? (
                  subcategories.map((sub) => (
                    <MenuItem key={sub.id} value={sub.id}>
                      {sub.name}
                    </MenuItem>
                  ))
                ) : (
                  <MenuItem disabled>No subcategory available</MenuItem>
                )}
              </Select>
            </FormControl>

            <Button variant="outlined" component="label">
              Upload Image
              <input
                type="file"
                hidden
                accept="image/*"
                onChange={(e) => setImage(e.target.files[0])}
              />
            </Button>

            <Button variant="contained" onClick={handleAddProduct}>
              Add Product
            </Button>
          </Box>

          {/* PRODUCT TABLE (NO API YET) */}
          <TableContainer component={Paper}>
            <Table>
              <TableHead sx={{ background: "#eee" }}>
                <TableRow>
                  <TableCell>#</TableCell>
                  <TableCell>Name</TableCell>
                  <TableCell>Price</TableCell>
                  <TableCell>Stock</TableCell>
                  <TableCell>Category</TableCell>
                  <TableCell>SubCategory</TableCell>
                  <TableCell>Image</TableCell>
                  <TableCell>Action</TableCell>
                </TableRow>
              </TableHead>

              <TableBody>
                {products.length === 0 && (
                  <TableRow>
                    <TableCell colSpan={8} align="center">
                      No Product Available
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

export default ProductManagement;
