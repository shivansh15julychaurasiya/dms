package ahc.dms.controller;

import ahc.dms.dao.dms.entities.CauseListType;
import ahc.dms.dao.dms.services.CauseListTypeService;
import ahc.dms.payload.response.ApiResponse;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.*;

@RestController
@RequestMapping("/dms/causelisttypes")
public class CauseListTypeController {

    @Autowired
    private CauseListTypeService service;

    @PostMapping
    public ResponseEntity<ApiResponse<CauseListType>> create(@RequestBody CauseListType causeListType) {
        CauseListType saved = service.create(causeListType);
        return ResponseEntity.ok(
                new ApiResponse<>(true, "Created successfully", null, saved, System.currentTimeMillis())
        );
    }

    @GetMapping
    public ResponseEntity<ApiResponse<List<CauseListType>>> getAllCauseList() {
        List<CauseListType> list = service.getAll();
        return ResponseEntity.ok(
                new ApiResponse<>(true, "Fetched all successfully", null, list, System.currentTimeMillis())
        );
    }

//    @GetMapping("/{id}")
//    public ResponseEntity<ApiResponse<CauseListType>> getById(@PathVariable Long id) {
//        CauseListType result = service.getById(id);
//        if (result != null) {
//            return ResponseEntity.ok(
//                    new ApiResponse<>(true, "Fetched successfully", null, result, System.currentTimeMillis())
//            );
//        } else {
//            return ResponseEntity.status(404).body(
//                    new ApiResponse<>(false, "CauseListType not found", null, null, System.currentTimeMillis())
//            );
//        }
//    }

    @PutMapping("/{id}")
    public ResponseEntity<ApiResponse<CauseListType>> update(@PathVariable Long id, @RequestBody CauseListType updated) {
        CauseListType result = service.update(id, updated);
        if (result != null) {
            return ResponseEntity.ok(
                    new ApiResponse<>(true, "Updated successfully", null, result, System.currentTimeMillis())
            );
        } else {
            return ResponseEntity.status(404).body(
                    new ApiResponse<>(false, "CauseListType not found for update", null, null, System.currentTimeMillis())
            );
        }
    }



}
