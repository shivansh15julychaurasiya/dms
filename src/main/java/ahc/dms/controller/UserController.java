package ahc.dms.controller;

import ahc.dms.config.AppConstants;
import ahc.dms.dao.dms.services.UserRoleService;
import ahc.dms.payload.ApiResponse;
import ahc.dms.payload.UserDto;
import ahc.dms.dao.dms.services.UserService;
import ahc.dms.utils.ResponseUtil;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.validation.Valid;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/dms/users")
public class UserController {

    @Autowired
    private UserService userService;
    @Autowired
    private UserRoleService userRoleService;

    @PostMapping("/create")
    public ResponseEntity<ApiResponse<UserDto>> createUser(@Valid @RequestBody UserDto userDto){

        UserDto createdUserDto = userService.createUser(userDto);
        return ResponseEntity.ok(ResponseUtil.success(createdUserDto, "user created"));

    }

    @PutMapping("/{userId}")
    public ResponseEntity<ApiResponse<UserDto>> updateUser(@Valid @RequestBody UserDto userDto, @PathVariable("userId") Long userId) {
        UserDto updatedUser = userService.updateUser(userDto, userId);
        return ResponseEntity.ok(ResponseUtil.success(updatedUser, "user updated"));
    }

    @PreAuthorize("hasRole('ADMIN')")
    @GetMapping("/deactivate/{username}")
    public ResponseEntity<ApiResponse<?>> deactivateUser(
            @PathVariable("username") String loginId,
            HttpServletRequest request
    ){
        userService.deactivateUser(loginId);
        return ResponseEntity.ok(ResponseUtil.success(null, "user deactivated"));
    }

    @PreAuthorize("hasRole('ADMIN')")
    @GetMapping("/activate/{username}")
    public ResponseEntity<ApiResponse<?>> activateUser(
            @PathVariable("username") String loginId,
            HttpServletRequest request
    ){
        userService.activateUser(loginId);
        return ResponseEntity.ok(ResponseUtil.success(null, "user activated"));
    }

    @GetMapping("/")
    public ResponseEntity<ApiResponse<List<UserDto>>> getAllUsers(
            @RequestParam(value = "pageNumber", defaultValue = AppConstants.PAGE_NUMBER, required = false) Integer pageNumber,
            @RequestParam(value = "pageSize", defaultValue = AppConstants.PAGE_SIZE, required = false) Integer pageSize,
            @RequestParam(value = "sortBy", defaultValue = AppConstants.SORT_USER_BY, required = false) String sortBy,
            @RequestParam(value = "sortDir", defaultValue = AppConstants.SORT_DIR, required = false) String sortDir
    ){
        //return ResponseEntity.of(Optional.ofNullable(userService.getAllUsers()));
        return ResponseEntity.ok(ResponseUtil.success(userService.getAllUsers(pageNumber, pageSize, sortBy, sortDir), "user fetched"));
    }

    @GetMapping("/{userId}")
    public ResponseEntity<ApiResponse<UserDto>> getSingleUser(@PathVariable("userId") Long userId){
        //return ResponseEntity.ok(userService.getUserById(userId));
        return ResponseEntity.ok(ResponseUtil.success(userService.getUserById(userId), "user fetched"));
    }

}
