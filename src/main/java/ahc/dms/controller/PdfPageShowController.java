package ahc.dms.controller;

import ahc.dms.dao.dms.entities.CaseFileDetails;
import ahc.dms.dao.dms.services.CaseFileDetailsService;
import ahc.dms.payload.response.ApiResponse;
import org.springframework.http.*;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.io.FileNotFoundException;
import java.time.Instant;
import java.util.List;

@RestController
@RequestMapping("dms/api")
public class PdfPageShowController {
 private CaseFileDetailsService service;


    @GetMapping("/getPdf")
    public ResponseEntity<byte[]> serchCasePdf(
            @RequestParam(required = false) String caseType,
            @RequestParam(required = false) String caseNo,
            @RequestParam(required = false) Integer caseYear) throws Exception {



//        System.out.println("From Controller-> Casetype="+caseTypeId+" "+"CaseNO="+caseNo+" "+"CaseYear="+caseYear);
//
//        List<CaseFileDetails> result = service.searchCases(caseTypeId, caseNo, caseYear);
//
//
//        ApiResponse<List<CaseFileDetails>> response = new ApiResponse<>();
//        response.setStatus(true);
//        response.setMessage("Search result fetched successfully");
//        response.setData(result);
//        response.setTimestamp(Instant.now().toEpochMilli());
//
//        return ResponseEntity.ok(response);

       try {
            byte[] pdfBytes = service.getTop10PagesPdf(caseType, caseNo, caseYear);

            HttpHeaders headers = new HttpHeaders();
            headers.setContentType(MediaType.APPLICATION_PDF);
            headers.setContentDisposition(ContentDisposition.builder("inline")
                    .filename("top-10-pages.pdf").build());

            return new ResponseEntity<>(pdfBytes, headers, HttpStatus.OK);
        } catch (FileNotFoundException e) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(null);
        } catch (Exception e) {
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(null);
        }

    }


}
