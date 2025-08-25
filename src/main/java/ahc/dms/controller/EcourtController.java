package ahc.dms.controller;

import ahc.dms.dao.dms.entities.*;
import ahc.dms.dao.dms.services.CauseListService;
import ahc.dms.dao.dms.services.RequestLogService;
import ahc.dms.dao.dms.services.TokenLogService;
import ahc.dms.dao.dms.services.UserRoleService;
import ahc.dms.payload.response.ApiResponse;
import ahc.dms.security.JwtHelper;
import ahc.dms.utils.ResponseUtil;
import jakarta.servlet.http.HttpServletRequest;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

@RestController
@RequestMapping("/dms/ecourt")
public class EcourtController {

    @Autowired
    private RequestLogService requestLogService;
    @Autowired
    private UserDetailsService userDetailsService;
    @Autowired
    private TokenLogService tokenLogService;
    @Autowired
    private JwtHelper jwtHelper;
    @Autowired
    private UserRoleService userRoleService;
    @Autowired
    private CauseListService causeListService;


    @GetMapping("/getReport")
    @PreAuthorize("hasAnyRole('ADMIN', 'USER','ECOURT')")
    public ResponseEntity<ApiResponse<List<CauseList>>> getDashBoardReport(HttpServletRequest request){
        requestLogService.logRequest(request);
        String authHeader = request.getHeader("Authorization");
        String token = authHeader.substring(7);
        String username = this.jwtHelper.getUsernameFromToken(token);

        User userDetails = (User) this.userDetailsService.loadUserByUsername(username);
        UserRole ur = userRoleService.getRoleByUserId(userDetails.getUserId());
        System.out.println("userroel========="+ur.getRole());
        LocalDate date = LocalDate.now();
        CauseList causeList=new CauseList();
        List<Object[]> list = new ArrayList<>();
        if(ur.getRole().getRoleId()==3){
            CourtUserMapping cum = causeListService.getCourtMapping(userDetails.getUserId());
            causeList.setCl_court_no(cum.getCumCourtMid());
            causeList.setCl_dol(date);
            list = causeListService.getListByType(causeList);
            System.out.println("CauseList list======"+list);
        } else {
            list = causeListService.getListByType(causeList);
        }
        List<CauseList> clList = new ArrayList<>();
        for(int i = 0; i< list.size(); i++){
            CauseList c = new CauseList();
            Object[] row1 = (Object[]) list.get(i);
            if(row1[1].toString().equals("1")){
                c.setCl_list_type_mid(1L);
                c.setCount(Integer.parseInt(row1[0].toString()));
                c.setClTypeData((String) row1[2].toString());

                System.out.println("CaseType count====="+c);
            }
            if(row1[1].toString().equals("2")){
                c.setCl_list_type_mid(2L);
                c.setCount(Integer.parseInt(row1[0].toString()));
                c.setClTypeData((String) row1[2].toString());
            }
            if(row1[1].toString().equals("3")){
                c.setCl_list_type_mid(3L);
                c.setCount(Integer.parseInt(row1[0].toString()));
                c.setClTypeData((String) row1[2].toString());
            }
            if(row1[1].toString().equals("4")){
                c.setCl_list_type_mid(4L);
                c.setCount(Integer.parseInt(row1[0].toString()));
                c.setClTypeData((String) row1[2].toString());
            }
            if(row1[1].toString().equals("5")){
                c.setCl_list_type_mid(5L);
                c.setCount(Integer.parseInt(row1[0].toString()));
                c.setClTypeData((String) row1[2].toString());

            }
            if(row1[1].toString().equals("6")){
                c.setCl_list_type_mid(6L);
                c.setCount(Integer.parseInt(row1[0].toString()));
                c.setClTypeData((String) row1[2].toString());
            }
            if(row1[1].toString().equals("7")){
                c.setCl_list_type_mid(7L);
                c.setCount(Integer.parseInt(row1[0].toString()));
                c.setClTypeData((String) row1[2].toString());

            }
            if(row1[1].toString().equals("19")){
                c.setCl_list_type_mid(19L);
                c.setCount(Integer.parseInt(row1[0].toString()));
                c.setClTypeData((String) row1[2].toString());
            }
            clList.add(c);
        }

    return ResponseEntity.ok(ResponseUtil.success(clList,"CauseListReport Data is Successful"));
    }


}
