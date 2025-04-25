package ahc.dms.controller;

import ahc.dms.payload.ApiResponse;
import ahc.dms.payload.UserDto;
import ahc.dms.dao.services.UserService;
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

    @PostMapping("/create")
    public ResponseEntity<ApiResponse<UserDto>> createUser(@Valid @RequestBody UserDto userDto){

        UserDto createdUserDto = userService.createUser(userDto);
        return ResponseEntity.ok(ResponseUtil.success(createdUserDto, "user created"));

    }

//    @PostMapping("/assign-roles")
//    public ResponseEntity<ApiResponse<UserDto>> assignRoleToUser(@Valid @RequestBody UserDto userDto){
//
//        UserDto updatedDto = userService.assignRoles(userDto);
//        return ResponseEntity.ok(ResponseUtil.success(updatedDto, "user created"));
//
//    }

    @PutMapping("/{userId}")
    public ResponseEntity<ApiResponse<UserDto>> updateUser(@Valid @RequestBody UserDto userDto, @PathVariable("userId") Integer userId) {
        UserDto updatedUser = userService.updateUser(userDto, userId);
        return ResponseEntity.ok(ResponseUtil.success(updatedUser, "user updated"));
    }

    @PreAuthorize("hasRole('ADMIN')")
    @DeleteMapping("/{userId}")
    public ResponseEntity<ApiResponse<?>> deleteUser(
            @PathVariable("userId") Integer userId,
            HttpServletRequest request
    ){
        userService.deleteUser(userId);
        return ResponseEntity.ok(ResponseUtil.success(null, "user deleted"));
    }

    @GetMapping("/")
    public ResponseEntity<ApiResponse<List<UserDto>>> getAllUsers(){
        //return ResponseEntity.of(Optional.ofNullable(userService.getAllUsers()));
        return ResponseEntity.ok(ResponseUtil.success(userService.getAllUsers(), "user fetched"));
    }

    @GetMapping("/{userId}")
    public ResponseEntity<ApiResponse<UserDto>> getSingleUser(@PathVariable("userId") Integer userId){
        //return ResponseEntity.ok(userService.getUserById(userId));
        return ResponseEntity.ok(ResponseUtil.success(userService.getUserById(userId), "user fetched"));
    }

}
