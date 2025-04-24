package ahc.dms.controller;

import ahc.dms.dao.services.RoleService;
import ahc.dms.payload.ApiResponse;
import ahc.dms.payload.RoleDto;
import ahc.dms.utils.ResponseUtil;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

@RestController
@RequestMapping("/dms/role")
public class RoleController {

    @Autowired
    private RoleService roleService;

    @PreAuthorize("hasRole('ADMIN')")
    @GetMapping("/")
    public ResponseEntity<ApiResponse<List<RoleDto>>> getAllRoles(){

        List<RoleDto> roles = roleService.getAllRoles();

        return ResponseEntity.ok(ResponseUtil.success(roles, "role list"));
    }

}
