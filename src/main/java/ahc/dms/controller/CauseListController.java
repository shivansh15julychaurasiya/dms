package ahc.dms.controller;

import ahc.dms.dao.dms.entities.CauseList;
import ahc.dms.dao.dms.services.CauseListService;
import ahc.dms.payload.request.CauseListSearchRequest;
import ahc.dms.payload.response.ApiResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.bind.annotation.*;

import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Date;
import java.util.List;


@RestController
@RequestMapping("/dms/cause-lists")
public class CauseListController {

//  ********************** Fullstack Java Developer Vijay Chaurasiya *******************************

	
    @Autowired
    private CauseListService causeListService;

    private static final Logger logger = LoggerFactory.getLogger(CauseListController.class);

    @GetMapping("/search")
    @PreAuthorize("hasAnyRole('ADMIN', 'USER')")
    public ResponseEntity<ApiResponse<List<CauseList>>> search(
            @RequestParam(required = false) Integer courtNo,
            @RequestParam(required = false) Long listTypeId,
            @RequestParam(required = false) @DateTimeFormat(pattern = "yyyy-MM-dd") String dol
    ) {
        logger.info("Searching cause list with courtNo: {}, listTypeId: {}, dol: {}", courtNo, listTypeId, dol);

         LocalDate  localDate = LocalDate.parse(dol);


        List<CauseList> result = causeListService.searchCauseLists(courtNo, listTypeId, localDate);

        ApiResponse<List<CauseList>> response = new ApiResponse<>(
                true,
                "Cause list fetched successfully",
                null,
                result,
                Instant.now().toEpochMilli()
        );

        return ResponseEntity.ok(response);
    }
}


