package ahc.dms.dao.dms.repositories;

import ahc.dms.dao.dms.entities.CauseListType;
import ahc.dms.dao.dms.services.CauseListTypeService;
import ahc.dms.payload.response.ApiResponse;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.*;

@RestController
@RequestMapping("/dms/causelisttype")
public class CauseListTypeController {

    @Autowired
    private CauseListTypeService service;

    @GetMapping
    public ResponseEntity<ApiResponse<List<CauseListType>>> getAll() {
        List<CauseListType> list = service.getAll();
        return ResponseEntity.ok(new ApiResponse<>(true, "Fetched successfully", null, list, System.currentTimeMillis()));
    }


}