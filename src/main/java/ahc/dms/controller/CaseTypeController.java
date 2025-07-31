package ahc.dms.controller;

import ahc.dms.dao.dms.entities.CaseType;
import ahc.dms.dao.dms.services.CaseTypeService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

@RestController
@RequestMapping("/dms/caseTypes")
public class CaseTypeController {

    @Autowired
    private CaseTypeService cts;


    @GetMapping
    @PreAuthorize("hasAnyRole('ADMIN','USER')")
   ResponseEntity<List<CaseType>> getAllCasetype(){

       return ResponseEntity.ok((cts.getCaseTypes()));

   }

}
