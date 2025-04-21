import "./table.scss";
import Table from '@mui/material/Table';
import TableBody from '@mui/material/TableBody';
import TableCell from '@mui/material/TableCell';
import TableContainer from '@mui/material/TableContainer';
import TableHead from '@mui/material/TableHead';
import TableRow from '@mui/material/TableRow';
import Paper from '@mui/material/Paper';
import React, { useEffect, useState } from 'react';
import axios from 'axios';
import Pagination from '@mui/material/Pagination';
import Stack from '@mui/material/Stack';

export default function TableData() {
  const [products, setProducts] = useState([]);
  const [page, setPage] = useState(1);
  const rowsPerPage = 5;

  useEffect(() => {
    axios.get("https://fakestoreapi.com/products")
      .then(res => setProducts(res.data))
      .catch(err => console.log(err));
  }, []);

  const handleChange = (event, value) => {
    setPage(value);
  };

  // Calculate the paginated data
  const paginatedData = products.slice((page - 1) * rowsPerPage, page * rowsPerPage);

  return (
    <>
      <TableContainer component={Paper} className="table">
        <Table sx={{ minWidth: 650 }} aria-label="simple table">
          <TableHead >
            <TableRow className="bg-secondary ">
              <TableCell className="tableCell text-light font-weight-bold">SR.NO</TableCell>
              <TableCell className="tableCell text-light font-weight-bold">Image</TableCell>
              <TableCell className="tableCell text-light font-weight-bold">Price</TableCell>
              <TableCell className="tableCell text-light font-weight-bold">Description</TableCell>
              <TableCell className="tableCell text-light font-weight-bold">Category</TableCell>
              <TableCell className="tableCell text-light font-weight-bold">Product</TableCell>
              <TableCell className="tableCell text-light font-weight-bold">Rating</TableCell>
            </TableRow>
          </TableHead>
          <TableBody>
            {paginatedData.map((product) => (
              <TableRow key={product.id}>
                <TableCell className="tableCell">{product.id}</TableCell>
                <TableCell className="tableCell">
                  <div className="cellWrapper">
                    <img src={product.image} className="image" alt={product.title} />
                  </div>
                </TableCell>
                <TableCell className="tableCell">${product.price}</TableCell>
                <TableCell className="tableCell">{product.description}</TableCell>
                <TableCell className="tableCell">{product.category}</TableCell>
                <TableCell className="tableCell">{product.title}</TableCell>
                <TableCell className="tableCell">{product.rating.rate}</TableCell>
              </TableRow>
            ))}
          </TableBody>
        </Table>
      </TableContainer>

      {/* Pagination Controls */}
      <Stack spacing={2} mt={2} alignItems="center">
        <Pagination 
          count={Math.ceil(products.length / rowsPerPage)} 
          page={page} 
          onChange={handleChange} 
          color="primary"
        />
      </Stack>
    </>
  );
}
