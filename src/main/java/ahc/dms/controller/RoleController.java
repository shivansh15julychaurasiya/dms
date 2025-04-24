package ahc.dms.controller;

import ahc.dms.dao.services.RoleService;
import ahc.dms.payload.ApiResponse;
import ahc.dms.payload.RoleDto;
import ahc.dms.utils.ResponseUtil;
import jakarta.validation.Valid;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/dms/role")
public class RoleController {

    @Autowired
    private RoleService roleService;

    @PreAuthorize("hasRole('ADMIN')")
    @PostMapping("/")
    public ResponseEntity<ApiResponse<RoleDto>> createRole(@Valid @RequestBody RoleDto roleDto){
        RoleDto createdRoleDto = roleService.createRole(roleDto);
        return ResponseEntity.ok(ResponseUtil.success(createdRoleDto, "role created"));

    }

    @PutMapping("/{roleId}")
    public ResponseEntity<ApiResponse<RoleDto>> updateRole(@Valid @RequestBody RoleDto roleDto, @PathVariable("roleId") Integer roleId) {
        RoleDto updatedRole = roleService.updateRole(roleDto, roleId);
        return ResponseEntity.ok(ResponseUtil.success(updatedRole, "role updated"));
    }

    @PreAuthorize("hasRole('ADMIN')")
    @GetMapping("/")
    public ResponseEntity<ApiResponse<List<RoleDto>>> getAllRoles(){
        List<RoleDto> roles = roleService.getAllRoles();
        return ResponseEntity.ok(ResponseUtil.success(roles, "role list"));
    }

    @PreAuthorize("hasRole('ADMIN')")
    @DeleteMapping("/{roleId}")
    public ResponseEntity<ApiResponse<?>> deleteRole(@PathVariable("roleId") Integer roleId){
        roleService.deleteRole(roleId);
        return ResponseEntity.ok(ResponseUtil.success(null, "role deleted"));
    }

}
