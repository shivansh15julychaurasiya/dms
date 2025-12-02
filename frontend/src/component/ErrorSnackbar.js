// components/ErrorSnackbar.js
import { Snackbar, Alert } from "@mui/material";

export default function ErrorSnackbar({ open, message, onClose }) {
  return (
    <Snackbar open={open} autoHideDuration={4000} onClose={onClose}>
      <Alert severity="error" onClose={onClose} variant="filled">
        {message}
      </Alert>
    </Snackbar>
  );
}
