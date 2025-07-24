package ahc.dms.controller;

import ahc.dms.dao.dms.entities.CauseList;
import ahc.dms.dao.dms.services.CauseListService;
import ahc.dms.payload.request.CauseListSearchRequest;
import ahc.dms.payload.response.ApiResponse;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.bind.annotation.*;

import java.time.Instant;
import java.util.List;

@RestController
@RequestMapping("/dms/cause-list")
public class CauseListController {

    private final CauseListService causeListService;

    public CauseListController(CauseListService causeListService) {
        this.causeListService = causeListService;
    }

    @PostMapping("/search")
    public ResponseEntity<List<CauseList>> searchCauseLists(@RequestBody CauseListSearchRequest request) {
        List<CauseList> result = causeListService.searchCauseLists(
                request.getCourtName(),
                request.getCauseListDesc(),
                request.getDate()
        );
        return ResponseEntity.ok(result);
    }
}
