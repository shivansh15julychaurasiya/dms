package ahc.dms.controller;

import ahc.dms.dao.dms.entities.Documents;
import ahc.dms.dao.dms.services.DocumentService;
import ahc.dms.payload.response.ApiResponse;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import java.security.Principal;
import java.util.List;

@RestController
@RequestMapping("/dms/api/documents")
public class DocumentController {

    @Autowired
    private DocumentService documentService;

    @PostMapping("/upload")
    public ResponseEntity<?> uploadDocument(
            @RequestParam("file") MultipartFile file,
            @RequestParam("title") String title,
            @RequestParam("type") String fileType,
            @RequestParam("caseType") String caseType,
            @RequestParam("caseNumber") String caseNumber,
            @RequestParam("caseYear") String caseYear,
            @RequestParam("uploadDate") String uploadDate,
            Principal principal
    ) {
        try {
            String uploadedBy = principal.getName();
            Documents saved = documentService.uploadDocument(file, title, fileType, caseType, caseNumber, caseYear, uploadDate, uploadedBy);
            return ResponseEntity.ok(saved);
        } catch (Exception e) {
            return ResponseEntity.status(500).body("Upload failed: " + e.getMessage());
        }
    }

    @GetMapping("/search")
    public ResponseEntity<ApiResponse<List<Documents>>> searchDocuments(
            @RequestParam(required = false) String caseType,
            @RequestParam(required = false) String caseNo,
            @RequestParam(required = false) String caseYear
    ) {
        List<Documents> result = documentService.searchDocuments(caseType, caseNo, caseYear);

        ApiResponse<List<Documents>> response = new ApiResponse<>(
                true,
                "Documents fetched successfully",
                null,
                result,
                System.currentTimeMillis()
        );

        return ResponseEntity.ok(response);
    }


}
