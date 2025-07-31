package ahc.dms.controller;

import ahc.dms.dao.dms.entities.CaseFileDetails;
import ahc.dms.dao.dms.services.CaseFileDetailsService;
import ahc.dms.payload.response.ApiResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.core.io.Resource;
import org.springframework.core.io.UrlResource;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RestController;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.Instant;
import java.util.List;

@RestController
@RequestMapping("/dms/api/casesfiles")
@RequiredArgsConstructor
public class CaseFileDetailsController {

    private final CaseFileDetailsService service;

    @GetMapping("/search")
    public ResponseEntity<ApiResponse<List<CaseFileDetails>>> searchCases(
            @RequestParam(required = false) Integer caseTypeId,
            @RequestParam(required = false) String caseNo,
            @RequestParam(required = false) Integer caseYear) {

        List<CaseFileDetails> result = service.searchCases(caseTypeId, caseNo, caseYear);

        ApiResponse<List<CaseFileDetails>> response = new ApiResponse<>();
        response.setStatus(true);
        response.setMessage("Search result fetched successfully");
        response.setData(result);
        response.setTimestamp(Instant.now().toEpochMilli());

        return ResponseEntity.ok(response);
    }

    @GetMapping("/documents/view/{docName}")
    public ResponseEntity<Resource> viewPDF(@PathVariable String docName) throws IOException {

        Path filePath = Paths.get("D:/pdf-storage/" + docName + ".pdf");

        System.out.println("Looking for: " + filePath.toString());
        System.out.println("Exists: " + Files.exists(filePath));


        if (!Files.exists(filePath)) {
            return ResponseEntity.notFound().build();
        }

        Resource resource = new UrlResource(filePath.toUri());

        return ResponseEntity.ok()
                .contentType(MediaType.APPLICATION_PDF)
                .header(HttpHeaders.CONTENT_DISPOSITION, "inline; filename=" + docName + ".pdf")
                .body(resource);
    }




    @GetMapping("/{id}")
    public ResponseEntity<ApiResponse<CaseFileDetails>> getCase(@PathVariable Long id) {
        CaseFileDetails caseFile = service.getCaseFile(id);

        ApiResponse<CaseFileDetails> response = new ApiResponse<>();
        response.setStatus(true);
        response.setMessage("Case file fetched successfully");
        response.setData(caseFile);
        response.setTimestamp(Instant.now().toEpochMilli());

        return ResponseEntity.ok(response);
    }
}