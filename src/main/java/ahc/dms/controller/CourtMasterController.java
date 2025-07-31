package ahc.dms.controller;

import ahc.dms.dao.dms.entities.CourtMaster;
import ahc.dms.dao.dms.services.CourtMasterService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

@RestController
@RequestMapping("/dms/courtMaster")
public class CourtMasterController {

    @Autowired
    private CourtMasterService cms;

    @GetMapping("/")
    @PreAuthorize("hasAnyRole('ADMIN','USER')")
    ResponseEntity<List<CourtMaster>> getAllCourts(){

        return ResponseEntity.ok(cms.getCourtMaster());
    }
}
