package ahc.dms.controller;

import ahc.dms.config.AppConstants;
import ahc.dms.dao.services.OtpLogService;
import ahc.dms.payload.*;
import ahc.dms.dao.services.TokenService;
import ahc.dms.dao.services.UserService;
import ahc.dms.security.JwtTokenHelper;
import ahc.dms.utils.OtpHelper;
import ahc.dms.utils.ResponseUtil;
import jakarta.servlet.http.HttpServletRequest;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.web.bind.annotation.*;

import java.time.LocalDateTime;
import java.util.Random;

@RestController
@RequestMapping("/dms/auth")
public class AuthController {

    @Autowired
    private JwtTokenHelper jwtTokenHelper;
    @Autowired
    private OtpHelper otpHelper;
    @Autowired
    private UserDetailsService userDetailsService;
    @Autowired
    private TokenService tokenService;
    @Autowired
    private OtpLogService otpLogService;
    @Autowired
    private AuthenticationManager authenticationManager;
    @Autowired
    private UserService userService;
    private final Logger logger = LoggerFactory.getLogger(AuthController.class);

    @PostMapping("/login-password")
    public ResponseEntity<ApiResponse<JwtAuthResponse>> loginUsingPassword(@RequestBody JwtAuthRequest request) {

        logger.info("inside login-password controller");
        try {
            this.authenticationManager
                    .authenticate(new UsernamePasswordAuthenticationToken(
                            request.getUsername(),
                            request.getPassword()
                    ));
        } catch (BadCredentialsException e) {
            return ResponseEntity.ok(ResponseUtil.error("User is disabled"));
        }
        //returns anonymousUser since session creation policy is stateless
        UserDetails userDetails = this.userDetailsService.loadUserByUsername(request.getUsername());
        String token = this.jwtTokenHelper.generateToken(userDetails);

        JwtAuthResponse jwtAuthResponse = new JwtAuthResponse();
        jwtAuthResponse.setToken(token);
        jwtAuthResponse.setMessage(AppConstants.JWT_CREATED);

        return ResponseEntity.ok(ResponseUtil.success(jwtAuthResponse,AppConstants.JWT_CREATED));

    }

    @PostMapping("/login-otp")
    public ResponseEntity<ApiResponse<JwtAuthResponse>> loginUsingOtp(@RequestBody JwtAuthRequest request) {

        logger.info("inside login-otp controller");

        boolean authStatus = otpLogService.verifyLoginOtp(request.getUsername(), request.getOtp());
        if (authStatus) {
            //returns anonymousUser since session creation policy is stateless
            UserDetails userDetails = this.userDetailsService.loadUserByUsername(request.getUsername());
            String token = this.jwtTokenHelper.generateToken(userDetails);

            JwtAuthResponse jwtAuthResponse = new JwtAuthResponse();
            jwtAuthResponse.setToken(token);
            jwtAuthResponse.setMessage(AppConstants.JWT_CREATED);
            return ResponseEntity.ok(ResponseUtil.success(jwtAuthResponse, "Logged-in successful"));
        }
        return ResponseEntity.ok(ResponseUtil.error("Invalid otp"));
    }

    @PostMapping("/register")
    public ResponseEntity<ApiResponse<UserDto>> registerUser(@RequestBody UserDto userDto) {
        UserDto registeredUserDto = userService.registerNewUser(userDto);
        return ResponseEntity.ok(ResponseUtil.success(registeredUserDto, "User created successfully"));
    }

    @GetMapping("/logout")
    public ResponseEntity<ApiResponse<JwtAuthResponse>> logoutUser(HttpServletRequest request) {

        String authHeader = request.getHeader("Authorization");
        String token = authHeader.substring(7);
        String username = this.jwtTokenHelper.getUsernameFromToken(token);
        TokenDto tokenDto = tokenService.findToken(token, username);
        TokenDto revokedToken = tokenService.revokeToken(tokenDto.getTokenId());

        JwtAuthResponse jwtAuthResponse = new JwtAuthResponse();
        jwtAuthResponse.setToken(revokedToken.getJwtToken());
        jwtAuthResponse.setMessage(AppConstants.JWT_REVOKED);

        return ResponseEntity.ok(ResponseUtil.success(jwtAuthResponse, "Logged out successfully"));
    }

    //@PreAuthorize("hasRole('ADMIN')")
    @PostMapping("/reset-password")
    public ResponseEntity<ApiResponse<?>> resetUserPassword(@RequestBody UserDto userDto) {
        UserDto updatedUser = userService.resetPassword(userDto.getLoginId(), userDto.getPassword());
        return ResponseEntity.ok(ResponseUtil.success(null, "Password has been reset"));
    }

    @PostMapping("/request-otp")
    public ResponseEntity<ApiResponse<OtpDto>> loginOtp(@RequestBody OtpDto requestOtp) {
        System.out.println("otpDto : " + requestOtp);
        String otp = String.valueOf(new Random().nextInt(9000) + 1000);
        UserDto savedUser = userService.getUserByLoginId(requestOtp.getLoginId());
        OtpDto otpLog = otpLogService.getOtpLogByLoginIdAndOtpType(requestOtp.getLoginId(), requestOtp.getOtpType());
        otpHelper.sendLoginOtp(savedUser.getPhone(), otp);
        otpLog.setOtpValue(otp);
        otpLog.setLoginId(requestOtp.getLoginId());
        otpLog.setPhone(requestOtp.getPhone());
        otpLog.setOtpType(requestOtp.getOtpType());
        otpLog.setOtpStatus(true);
        otpLog.setOtpExpiry(LocalDateTime.now());
        otpLogService.saveOtp(otpLog);
        return ResponseEntity.ok(ResponseUtil.success(null, "otp sent successfully"));
    }

    @PostMapping("/verify-reset-otp")
    public ResponseEntity<ApiResponse<?>> verifyResetOtp(@RequestBody JwtAuthRequest request) {

        logger.info("inside verify-reset-otp controller");
        boolean authStatus = otpLogService.verifyResetOtp(request.getUsername(), request.getOtp());
        if (authStatus) {
            return ResponseEntity.ok(ResponseUtil.success(null, "Otp verified successfully"));
        }
        return ResponseEntity.ok(ResponseUtil.error("Invalid otp"));
    }

}
