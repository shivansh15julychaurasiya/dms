package ahc.dms.controller;

import ahc.dms.dao.dms.entities.CourtMaster;
import ahc.dms.dao.dms.services.CourtMasterService;
import ahc.dms.payload.request.BenchUpdateRequest;
import ahc.dms.payload.response.ApiResponse;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.Date;
import java.util.List;

@RestController
@RequestMapping("/dms/court-master-type")
public class CourtMasterController {

    @Autowired
    private CourtMasterService service;

    @PostMapping("/newcourt")
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<ApiResponse<CourtMaster>> create(@RequestBody CourtMaster courtMaster) {


        int totalRecordsInCourtMaster=(int)service.getCourtMasterCount();
        System.out.println("***************************"+totalRecordsInCourtMaster);

        courtMaster.setCm_value(totalRecordsInCourtMaster-2);
        courtMaster.setCm_cr_date(new Date());


        CourtMaster saved = service.saveCourtMaster(courtMaster);
        return ResponseEntity.ok(new ApiResponse<>(true, "Court master created", null, saved, System.currentTimeMillis()));

    }

    @GetMapping
    @PreAuthorize("hasAnyRole('ADMIN','USER')")
    public ResponseEntity<ApiResponse<List<CourtMaster>>> getAll() {
        List<CourtMaster> list = service.getAllCourtMasters();
        return ResponseEntity.ok(new ApiResponse<>(true, "Court master list fetched", null, list, System.currentTimeMillis()));
    }


    @PutMapping("/update-bench")
    @PreAuthorize("hasAnyRole('ADMIN')")
    public ResponseEntity<ApiResponse<CourtMaster>> updateCourtBench(@RequestBody BenchUpdateRequest request) {
        CourtMaster updated = service.updateBenchId(request.getId(), request.getBenchId());
        return ResponseEntity.ok(
                new ApiResponse<>(true, "Bench ID updated", null, updated, System.currentTimeMillis())
        );
    }



    @DeleteMapping("/{id}")
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<ApiResponse<Void>> delete(@PathVariable Integer id) {
        service.deleteCourtMaster(id);
        return ResponseEntity.ok(new ApiResponse<>(true, "Court master deleted", null, null, System.currentTimeMillis()));
    }
}
