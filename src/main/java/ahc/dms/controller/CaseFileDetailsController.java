package ahc.dms.controller;

import ahc.dms.dao.dms.entities.CaseFileDetails;
import org.springframework.core.io.Resource;
import org.springframework.core.io.UrlResource;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

@RestController
@RequestMapping("/dms/api/casesfiles")
public class CaseFileDetailsController {


    @GetMapping("/search")




    @GetMapping("/documents/view/{docName}")
    public ResponseEntity<Resource> viewPDF(@PathVariable String docName) throws IOException {


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
