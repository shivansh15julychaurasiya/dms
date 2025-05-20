package ahc.dms.controller;

import ahc.dms.dao.dms.services.*;
import ahc.dms.payload.request.ObjectRoleRequest;
import ahc.dms.payload.response.ApiResponse;
import ahc.dms.payload.dto.ObjectMasterDto;
import ahc.dms.payload.dto.ObjectRoleDto;
import ahc.dms.payload.response.ObjectRoleResponse;
import ahc.dms.utils.ResponseUtil;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.validation.Valid;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/dms/object")
public class ObjectController {

    @Autowired
    private ObjectRoleService orService;
    @Autowired
    private ObjectMasterService omService;
    @Autowired
    private RequestLogService requestLogService;

    @PostMapping("/register")
    public ResponseEntity<ApiResponse<ObjectRoleResponse>> createObjectRole(
            HttpServletRequest request,
            @Valid @RequestBody ObjectRoleRequest orRequest
    ){
        requestLogService.logRequest(request);
        ObjectRoleResponse objectRoleResponse = orService.createObjectRole(orRequest);
        return ResponseEntity.ok(ResponseUtil.success(objectRoleResponse, "object-role mapping created"));
    }

}
