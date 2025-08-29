package ahc.dms.controller;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.text.ParseException;
import java.time.Instant;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.io.FileUtils;
import org.springframework.core.io.InputStreamResource;
import org.springframework.core.io.Resource;
import org.springframework.core.io.UrlResource;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.client.RestTemplate;

import com.fasterxml.jackson.databind.ObjectMapper;

//import com.sun.org.apache.xerces.internal.impl.xpath.regex.ParseException;

import ahc.dms.dao.dms.entities.CaseFileDetails;
import ahc.dms.dao.dms.entities.Lookup;
import ahc.dms.dao.dms.entities.SubDocument;
import ahc.dms.dao.dms.entities.User;
import ahc.dms.dao.dms.repositories.UserRepository;
import ahc.dms.dao.dms.services.CaseFileDetailsService;
import ahc.dms.dao.dms.services.LookupService;
import ahc.dms.dao.dms.services.SubDocumentService;
import ahc.dms.exceptions.ResourceNotFoundException;
import ahc.dms.payload.response.ApiResponse;
import jakarta.servlet.ServletContext;
import lombok.RequiredArgsConstructor;


@RestController
@RequestMapping("/dms/api/casesfiles")
@RequiredArgsConstructor
public class CaseFileDetailsController {

	
	    private final CaseFileDetailsService service;
	    private final SubDocumentService subDocumentService;
	    private final CaseFileDetailsService caseFileDetailsService;
	    private final LookupService lookupService;
	    private final UserRepository userRepository;
        private final ServletContext servletContext;
 

    @GetMapping("/search")
    public ResponseEntity<ApiResponse<List<CaseFileDetails>>> searchCases(
            @RequestParam(required = false) Integer caseTypeId,
            @RequestParam(required = false) String caseNo,
            @RequestParam(required = false) Integer caseYear) {

        System.out.println("From Controller-> Casetype="+caseTypeId+" "+"CaseNO="+caseNo+" "+"CaseYear="+caseYear);

        List<CaseFileDetails> result = service.searchCases(caseTypeId, caseNo, caseYear);


        ApiResponse<List<CaseFileDetails>> response = new ApiResponse<>();
        response.setStatus(true);
        response.setMessage("Search result fetched successfully");
        response.setData(result);
        response.setTimestamp(Instant.now().toEpochMilli());

        return ResponseEntity.ok(response);
    }

//    @GetMapping("/documents/view/{docName}")
//    public ResponseEntity<Resource> viewPDF(@PathVariable String docName) throws IOException {
//
//        Path filePath = Paths.get("D:/pdf-storage/" + docName + ".pdf");
//
//        System.out.println("Looking for: " + filePath.toString());
//        System.out.println("Exists: " + Files.exists(filePath));
//
//
//        if (!Files.exists(filePath)) {
//            return ResponseEntity.notFound().build();
//        }
//
//        Resource resource = new UrlResource(filePath.toUri());
//
//        return ResponseEntity.ok()
//                .contentType(MediaType.APPLICATION_PDF)
//                .header(HttpHeaders.CONTENT_DISPOSITION, "inline; filename=" + docName + ".pdf")
//                .body(resource);
//    }

    @GetMapping("/documents/view/{id}")
    public ResponseEntity<?> viewDocumentById(@PathVariable("id") Long docId) {

        try {
            // Get current user
            Object principal = SecurityContextHolder.getContext().getAuthentication().getPrincipal();
            String username = (principal instanceof UserDetails)
                    ? ((UserDetails) principal).getUsername()
                    : principal.toString();

            User user = userRepository.findByUsername(username)
                    .orElseThrow(() -> new RuntimeException("User not found"));

            Long userId = user.getUserId();

            // Fetch subdocument and case details
            SubDocument subDocument = subDocumentService.getPetitionSubDocument(docId, 1)
                    .orElseThrow(() -> new ResourceNotFoundException("SubDocument not found with id: " + docId, null, userId));

            CaseFileDetails caseFileDetails = caseFileDetailsService.getCaseFileDetail(docId)
                    .orElseThrow(() -> new ResourceNotFoundException("CaseFileDetails not found with id: " + docId, null, userId));

            System.out.println(caseFileDetails);
            // Get REPOSITORYPATH from lookup
            Lookup lookupRepo = lookupService.getLookUpObject("REPOSITORYPATH");
            if (lookupRepo == null) {
                throw new ResourceNotFoundException("Lookup entry for REPOSITORYPATH not found", null, userId);
            }

            // Build the full path to the PDF
            String repositoryPath = lookupRepo.getLongname(); // Assuming this is an absolute path like "D:/repo"
            String fileName = subDocument.getSd_document_name() + ".pdf";
            String caseTypeLabel = caseFileDetails.getCaseType().getLabel();
            String indexFieldName = subDocument.getIndexField().getName();

            String srcPath = lookupRepo.getLongname() + File.separator + caseFileDetails.getCaseType().getLabel()
					+ File.separator + subDocument.getIndexField().getName() + File.separator
					+ subDocument.getSd_document_name() + ".pdf";

            Path filePath = Paths.get(srcPath);
            System.out.println("Resolved file path: " + filePath);
            
            File file = new File(srcPath);
            System.out.println("File exists? " + file.exists());
            System.out.println("Absolute path = " + file.getAbsolutePath());
            
            String uploadPath = servletContext.getRealPath("");
			File dest = new File(uploadPath + File.separator + "uploads" + File.separator
					+ subDocument.getSd_document_name() + ".pdf");

			try {
				FileUtils.copyFile(file, dest);
			} catch (IOException e) {
				e.printStackTrace();
			}

//        

            Resource resource = new UrlResource(filePath.toUri());
            return ResponseEntity.ok(caseFileDetails);
                    

        } catch (ResourceNotFoundException ex) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(
                    new ApiResponse<>(false, ex.getMessage(), null, null, System.currentTimeMillis())
            );
        } catch (Exception e) {
            e.printStackTrace();
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(
                    new ApiResponse<>(false, "Something went wrong", null, null, System.currentTimeMillis())
            );
        }
    }

  @GetMapping("/view/{id}")
  public ResponseEntity<Resource> viewPDF(@PathVariable("id") Long docId) throws IOException {

	  
	  // Fetch subdocument and case details
      SubDocument subDocument = subDocumentService.getPetitionSubDocument(docId, 1)
              .orElseThrow(() -> new ResourceNotFoundException("SubDocument not found with id: " + docId, null, null));

      CaseFileDetails caseFileDetails = caseFileDetailsService.getCaseFileDetail(docId)
              .orElseThrow(() -> new ResourceNotFoundException("CaseFileDetails not found with id: " + docId, null, null));

      System.out.println(caseFileDetails);
      // Get REPOSITORYPATH from lookup
      Lookup lookupRepo = lookupService.getLookUpObject("REPOSITORYPATH");
      if (lookupRepo == null) {
          throw new ResourceNotFoundException("Lookup entry for REPOSITORYPATH not found", null, null);
      }

      // Build the full path to the PDF
      String repositoryPath = lookupRepo.getLongname(); // Assuming this is an absolute path like "D:/repo"
      String fileName = subDocument.getSd_document_name() + ".pdf";
      String caseTypeLabel = caseFileDetails.getCaseType().getLabel();
      String indexFieldName = subDocument.getIndexField().getName();

      String srcPath = lookupRepo.getLongname() + File.separator + caseFileDetails.getCaseType().getLabel()
				+ File.separator + subDocument.getIndexField().getName() + File.separator
				+ subDocument.getSd_document_name() + ".pdf";

      Path filePath = Paths.get(srcPath);
      System.out.println("Resolved file path: " + filePath);
      
      File file = new File(srcPath);
      System.out.println("File exists? " + file.exists());
      System.out.println("Absolute path = " + file.getAbsolutePath());
      
      String uploadPath = servletContext.getRealPath("");
		File dest = new File(uploadPath + File.separator + "uploads" + File.separator
				+ subDocument.getSd_document_name() + ".pdf");

     

      Resource resource = new UrlResource(filePath.toUri());

      return ResponseEntity.ok()
              .contentType(MediaType.APPLICATION_PDF)
              .header(HttpHeaders.CONTENT_DISPOSITION, "inline; filename=" + subDocument.getSd_document_name() + ".pdf")
              .body(resource);
  }
    
    
    
//    @GetMapping("/{id}")
//    public ResponseEntity<ApiResponse<CaseFileDetails>> getCase(@PathVariable Long id) {
//        CaseFileDetails caseFile = service.getCaseFile(id);
//
//        ApiResponse<CaseFileDetails> response = new ApiResponse<>();
//        response.setStatus(true);
//        response.setMessage("Case file fetched successfully");
//        response.setData(caseFile);
//        response.setTimestamp(Instant.now().toEpochMilli());
//
//        return ResponseEntity.ok(response);
//    

//  @GetMapping("/getOrdersFromElegalix/{id}")
//  public List<SubDocument> getOrders(@PathVariable("id") Long fdId) {
//      return subDocumentService.getOrdersFromElegalix(fdId);
//  }
  
  @GetMapping("/getOrdersFromElegalix/{id}")
  public ResponseEntity<ApiResponse<List<SubDocument>>> getOrdersFromElegalix(@PathVariable("id") Long fdId) throws ParseException {
      List<SubDocument> subDocuments = subDocumentService.getOrdersFromElegalix(fdId);

      ApiResponse<List<SubDocument>> response = new ApiResponse<>(
              true,
              "Orders fetched successfully",
              null,
              subDocuments,
              System.currentTimeMillis()
      );

      return ResponseEntity.ok(response);
  }

//

  @GetMapping("/showfile/{id}")
  public ResponseEntity<Resource> showFile(@PathVariable("id") Long docId) {
      SubDocument subDocument = subDocumentService.getSubDocumentById(docId);

      if (subDocument == null) {
          return ResponseEntity.notFound().build();
      }

      CaseFileDetails caseFileDetail = caseFileDetailsService.getCaseFileDetails(subDocument.getSd_fd_mid());
      Lookup lookupRepo = lookupService.getLookUpObject("REPOSITORYPATH");

      String srcPath = lookupRepo.getLongname() + File.separator
              + caseFileDetail.getCaseType().getLabel()
              + File.separator + subDocument.getIndexField().getName()
              + File.separator + subDocument.getSd_document_name() + ".pdf";
      
      System.out.println("********************Source path**********************="+srcPath);

      File file = new File(srcPath);

      if (!file.exists()) {
          return ResponseEntity.status(HttpStatus.NOT_FOUND)
                  .body(null);
      }

      try {
          InputStreamResource resource = new InputStreamResource(new FileInputStream(file));

          return ResponseEntity.ok()
                  .contentType(MediaType.APPLICATION_PDF) // PDF viewer needs this
                  .header(HttpHeaders.CONTENT_DISPOSITION, "inline; filename=" + file.getName())
                  .body(resource);

      } catch (IOException e) {
          return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
      }
  }
  
  
  @GetMapping("/getOrderFromElegalix/{id}")
  public ResponseEntity<ApiResponse<Map<String, Object>>> getOrderFromElegalix(
          @PathVariable("id") Long subDocId) {
      try {
          System.out.println("Judgement Id=" + subDocId);

          String uploadDir = new File("src/main/resources/static/uploads").getAbsolutePath();
          String uploadPath = uploadDir + File.separator + subDocId + ".pdf";

          // download and save file
          downloadFile(subDocId, uploadPath);

          System.out.println("File saved at: " + uploadPath);
          
          
          //  use HashMap
          Map<String, Object> data = new HashMap<>();
          data.put("doc_id", subDocId);
          data.put("document_name", subDocId + ".pdf");
          data.put("isApplication", null);
          data.put("isImpugnedOrder", null);

          ApiResponse<Map<String, Object>> response = new ApiResponse<>(
                  true,
                  "Order fetched successfully",
                  null,
                  data,
                  System.currentTimeMillis()
          );

          return ResponseEntity.ok(response);

      } catch (Exception e) {
          e.printStackTrace();

          ApiResponse<Map<String, Object>> errorResponse = new ApiResponse<>(
                  false,
                  "Unable to fetch order from Elegalix",
                  null,
                  null,
                  System.currentTimeMillis()
          );

          return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(errorResponse);
      }
  }


  // Method to download PDF file
  private void downloadFile(Long id, String uploadPath) {
      try {
          RestTemplate restTemplate = new RestTemplate();
          HttpHeaders headers = new HttpHeaders();
          headers.setAccept(Collections.singletonList(MediaType.APPLICATION_OCTET_STREAM));
          headers.add("Accept", "*/*");

          HttpEntity<String> entity = new HttpEntity<>(headers);
          String externalUrl = "http://192.168.0.162:8080/elegalix_restapi2/api/judgment/" + id;

          ResponseEntity<byte[]> response = restTemplate.exchange(
                  externalUrl, HttpMethod.GET, entity, byte[].class);

          System.out.println("upload path%%%%%%%%%%%%%%%%%="+uploadPath);
          Files.write(Paths.get(uploadPath), response.getBody());
          System.out.println("File saved at: " + uploadPath);

      } catch (Exception e) {
          e.printStackTrace();
      }
  }
  
  // Get PDF by file name from localproject
//Get PDF by file name from local project
@GetMapping("/getPdf/{fileName}")
public ResponseEntity<Resource> getPdf(@PathVariable String fileName) {
   try {
       // Path to the uploads folder (inside static, externalize if needed)
       Path filePath = Paths.get("src/main/resources/static/uploads").resolve(fileName).normalize();

       if (!Files.exists(filePath)) {
           return ResponseEntity.notFound().build();
       }

       Resource resource = new UrlResource(filePath.toUri());

       return ResponseEntity.ok()
               .contentType(MediaType.APPLICATION_PDF)
               .header(HttpHeaders.CONTENT_DISPOSITION, "inline; filename=" + resource.getFilename())
               .body(resource);

   } catch (Exception e) {
       e.printStackTrace();
       return ResponseEntity.internalServerError().build();
   }
}

}

