package ahc.dms.controller;

import ahc.dms.dao.dms.entities.CaseType;
import ahc.dms.dao.dms.entities.CauseListType;
import ahc.dms.dao.dms.services.CaseTypeService;
import ahc.dms.dao.dms.services.CauseListTypeService;
import ahc.dms.payload.response.ApiResponse;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.time.Instant;
import java.util.List;

@RestController
@RequestMapping("/dms/api/casetypes")
public class CaseTypeController {

//  ********************** Fullstack Java Developer Vijay Chaurasiya *******************************

	
    @Autowired
    private CaseTypeService caseTypeService;

    @PreAuthorize("hasAnyRole('ADMIN','USER','DMSAdmin')")
    @GetMapping
    public ResponseEntity<ApiResponse<List<CaseType>>> getAll() {
        List<CaseType> caseTypes = caseTypeService.getAllCaseTypes();
        ApiResponse<List<CaseType>> response = new ApiResponse<>(
                true,
                "Case types fetched successfully",
                null,
                caseTypes,
                Instant.now().toEpochMilli()
        );
        return ResponseEntity.ok(response);
    }

 /*   @PreAuthorize("hasAnyRole('ADMIN', 'USER')")
    @GetMapping("/{id}")
    public ResponseEntity<ApiResponse<?>> getById(@PathVariable Integer id) {
        return caseTypeService.getById(id)
                .map(caseType -> ResponseEntity.ok(new ApiResponse<>(
                        true,
                        "Case type found",
                        null,
                        caseType,
                        Instant.now().toEpochMilli()
                )))
                .orElse(ResponseEntity.status(404).body(new ApiResponse<>(
                        false,
                        "Case type not found with ID: " + id,
                        null,
                        null,
                        Instant.now().toEpochMilli()
                )));
    }
*/
//    @PreAuthorize("hasAnyRole('ADMIN', 'USER')")
//    @GetMapping("/label/{label}")
//    public ResponseEntity<ApiResponse<?>> getByLabel(@PathVariable String label) {
//        return caseTypeService.getByLabel(label)
//                .map(caseType -> ResponseEntity.ok(new ApiResponse<>(
//                        true,
//                        "Case type found",
//                        null,
//                        caseType,
//                        Instant.now().toEpochMilli()
//                )))
//                .orElse(ResponseEntity.status(404).body(new ApiResponse<>(
//                        false,
//                        "Case type not found with label: " + label,
//                        null,
//                        null,
//                        Instant.now().toEpochMilli()
//                )));
//    }

    @RestController
    @RequestMapping("/dms/causelisttype")
    public static class CauseListTypeController {

        @Autowired
        private CauseListTypeService service;

        @GetMapping
        public ResponseEntity<ApiResponse<List<CauseListType>>> getAll() {
            List<CauseListType> list = service.getAll();
            return ResponseEntity.ok(new ApiResponse<>(true, "Fetched successfully", null, list, System.currentTimeMillis()));
        }


    }
}
