package ahc.dms.controller;

import ahc.dms.dao.dms.entities.CaseFileDetails;
import ahc.dms.dao.dms.services.CaseFileDetailService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.Resource;
import org.springframework.core.io.UrlResource;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

@RestController
@RequestMapping("/dms/api/casesfiles")
public class CaseFileDetailsController {

    @Autowired
    private CaseFileDetailService service;

    @GetMapping("/search")
    public ResponseEntity<List<CaseFileDetails>> searchCases(
            @RequestParam String caseType,
            @RequestParam Long caseNo,
            @RequestParam Integer caseYear){

        List<CaseFileDetails> results = service.searchCases(caseType, caseNo, caseYear);

        return ResponseEntity.ok(results);
    }


    @GetMapping("/documents/view/{docName}")
    public ResponseEntity<Resource> viewPDF(@PathVariable String docName) throws IOException {

        Path filePath = Paths.get("A:/pdf-storage/" + docName + ".pdf");

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





}
