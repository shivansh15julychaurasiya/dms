package ahc.dms.controller;

import ahc.dms.config.AppConstants;
import ahc.dms.dao.dms.entities.*;
import ahc.dms.dao.dms.services.*;
import ahc.dms.payload.dto.RoleDto;
import ahc.dms.payload.dto.TokenLogDto;
import ahc.dms.payload.request.CauseListSearchRequest;
import ahc.dms.payload.request.JwtAuthRequest;
import ahc.dms.payload.response.ApiResponse;
import ahc.dms.payload.response.JwtAuthResponse;
import ahc.dms.security.JwtHelper;
import ahc.dms.utils.ResponseUtil;
import ch.qos.logback.core.net.SyslogOutputStream;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpSession;
import org.apache.coyote.Response;
import org.apache.tomcat.util.http.parser.Authorization;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.web.bind.annotation.*;

import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.List;


@RestController
@RequestMapping("/dms/cause-lists")
public class CauseListController {

    @Autowired
    private CauseListService causeListService;
    @Autowired
    private RequestLogService requestLogService;
    @Autowired
    private UserDetailsService userDetailsService;
    @Autowired
    private TokenLogService tokenLogService;
    @Autowired
    private JwtHelper jwtHelper;
    @Autowired
    private RoleService roleService;
    @Autowired
    private UserRoleService userRoleService;

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
        System.out.println("REsult=="+result);
        ApiResponse<List<CauseList>> response = new ApiResponse<>(
                true,
                "Cause list fetched successfully",
                null,
                result,
                Instant.now().toEpochMilli()
        );

        return ResponseEntity.ok(response);
    }


    @GetMapping("/type/{listTypeId}")
    @PreAuthorize("hasAnyRole('ADMIN', 'USER','ECOURT')")
    public ResponseEntity<ApiResponse<CauseListType>> searchList(
            @PathVariable Long listTypeId
    ) {

        CauseListType listType = causeListService.searchListType(listTypeId);
        System.out.println("REsult=="+listTypeId);
        ApiResponse<CauseListType> response = new ApiResponse<>(
                true,
                "Cause list fetched successfully",
                null,
                listType,
                Instant.now().toEpochMilli()
        );

        return ResponseEntity.ok(response);
    }

    @GetMapping("/getCauseList/{listTypeId}")
    @PreAuthorize("hasAnyRole('ADMIN', 'USER','ECOURT')")
    public ResponseEntity<ApiResponse<List<CauseList>>> getUsernameFromToken(@PathVariable Long listTypeId,HttpServletRequest request){
        requestLogService.logRequest(request);
        String authHeader = request.getHeader("Authorization");
        String token = authHeader.substring(7);
        String username = this.jwtHelper.getUsernameFromToken(token);

        User userDetails = (User) this.userDetailsService.loadUserByUsername(username);
        UserRole ur = userRoleService.getRoleByUserId(userDetails.getUserId());
        System.out.println("userroel========="+ur.getRole());

      //  RoleDto userRoles = roleService.getRoleByRoleId(ur.getUrId().intValue());
     //   RoleDto userRoles = roleService.getRoleByRoleId(userDetails.getUserId().intValue());

        List<CauseList> causeList1 = new ArrayList<>();
        CauseList causeList=new CauseList();
        causeList.setCl_list_type_mid(listTypeId);
       // if(userRoles.getRoleName().equals("ROLE_ECOURT")){
            if(ur.getRole().getRoleId()==3){
            CourtUserMapping cum = causeListService.getCourtMapping(userDetails.getUserId());
            causeList.setCl_court_no(cum.getCumCourtMid());
            System.out.println("cunrtmappint ==="+cum);
            System.out.println("CauseListdata ===="+causeList);
        }

        causeList1= causeListService.getCauseList(causeList);

      //  System.out.println("userRole==="+userRoles);
        System.out.println("userdetails-----------"+username);
        System.out.println("UserName "+userDetails);

     //   CauseList causeList = causeListService.searchListType(userId);
      //  List<CauseList> list = causeListService.getList();
        return ResponseEntity.ok(ResponseUtil.success(causeList1,"CauseList is successful"));

    }










}


