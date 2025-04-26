package ahc.dms.controller;

import ahc.dms.dao.services.RoleService;
import ahc.dms.dao.services.UserRoleService;
import ahc.dms.payload.ApiResponse;
import ahc.dms.payload.RoleDto;
import ahc.dms.payload.UserDto;
import ahc.dms.payload.UserRoleDto;
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
    @Autowired
    private UserRoleService userRoleService;

    @PreAuthorize("hasRole('ADMIN')")
    @PostMapping("/")
    public ResponseEntity<ApiResponse<RoleDto>> createRole(@Valid @RequestBody RoleDto roleDto){
        RoleDto createdRoleDto = roleService.createRole(roleDto);
        return ResponseEntity.ok(ResponseUtil.success(createdRoleDto, "role created"));

    }

    @PreAuthorize("hasRole('ADMIN')")
    @PutMapping("/{roleId}")
    public ResponseEntity<ApiResponse<RoleDto>> updateRole(@Valid @RequestBody RoleDto roleDto, @PathVariable("roleId") Integer roleId) {
        RoleDto updatedRole = roleService.updateRole(roleDto, roleId);
        return ResponseEntity.ok(ResponseUtil.success(updatedRole, "role updated"));
    }

    @GetMapping("/")
    public ResponseEntity<ApiResponse<List<RoleDto>>> getRoles(){
        List<RoleDto> roles = roleService.getAllRoles();
        return ResponseEntity.ok(ResponseUtil.success(roles, "role list"));
    }

    @GetMapping("/roleId/{roleId}")
    public ResponseEntity<ApiResponse<RoleDto>> getRoleById(@PathVariable("roleId") Integer roleId) {
        RoleDto role = roleService.getRoleByRoleId(roleId);
        return ResponseEntity.ok(ResponseUtil.success(role, "role found"));
    }

    @GetMapping("/roleName/{roleName}")
    public ResponseEntity<ApiResponse<RoleDto>> getRoleByName(@PathVariable("roleName") String roleName) {
        RoleDto role = roleService.getRoleByRoleName(roleName);
        return ResponseEntity.ok(ResponseUtil.success(role, "role found"));
    }

    @PreAuthorize("hasRole('ADMIN')")
    @DeleteMapping("/{roleId}")
    public ResponseEntity<ApiResponse<?>> deleteRole(@PathVariable("roleId") Integer roleId){
        roleService.deleteRole(roleId);
        return ResponseEntity.ok(ResponseUtil.success(null, "role deleted"));
    }

    @PostMapping("/assign-role")
    public ResponseEntity<ApiResponse<UserDto>> assignRoleToUser(@Valid @RequestBody UserRoleDto userRoleDto){

        UserDto updatedUserDto = userRoleService.assignRole(userRoleDto);
        return ResponseEntity.ok(ResponseUtil.success(updatedUserDto, "role assigned"));

    }

}
