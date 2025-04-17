package ahc.dms.controller;

import ahc.dms.config.AppConstants;
import ahc.dms.dao.services.OtpLogService;
import ahc.dms.payload.*;
import ahc.dms.dao.services.TokenService;
import ahc.dms.dao.services.UserService;
import ahc.dms.exceptions.ApiException;
import ahc.dms.security.JwtTokenHelper;
import ahc.dms.utils.OtpHelper;
import ahc.dms.utils.ResponseUtil;
import com.fasterxml.jackson.core.JsonProcessingException;
import jakarta.servlet.http.HttpServletRequest;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.web.bind.annotation.*;

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

    @PostMapping("/login")
    public ResponseEntity<JwtAuthResponse> createToken(@RequestBody JwtAuthRequest request) {

        logger.info("inside login controller");
        try {
            this.authenticationManager
                    .authenticate(new UsernamePasswordAuthenticationToken(request.getUsername(), request.getPassword()));
        } catch (BadCredentialsException e) {
            logger.info("user is disabled");
            throw new ApiException("Invalid username or password");
        }
        //returns anonymousUser since session creation policy is stateless
        UserDetails userDetails = this.userDetailsService.loadUserByUsername(request.getUsername());
        String token = this.jwtTokenHelper.generateToken(userDetails);

        JwtAuthResponse jwtAuthResponse = new JwtAuthResponse();
        jwtAuthResponse.setToken(token);
        jwtAuthResponse.setMessage(AppConstants.JWT_CREATED);

        return new ResponseEntity<>(jwtAuthResponse, HttpStatus.OK);

    }

    @PostMapping("/register")
    public ResponseEntity<UserDto> registerUser(@RequestBody UserDto userDto) {
        UserDto registeredUserDto = userService.registerNewUser(userDto);
        return new ResponseEntity<>(registeredUserDto, HttpStatus.CREATED);
    }

    @GetMapping("/logout")
    public ResponseEntity<JwtAuthResponse> logoutUser(HttpServletRequest request) {

        String authHeader = request.getHeader("Authorization");
        String token = authHeader.substring(7);
        String username = this.jwtTokenHelper.getUsernameFromToken(token);
        TokenDto tokenDto = tokenService.findToken(token, username);
        TokenDto revokedToken = tokenService.revokeToken(tokenDto.getTokenId());

        JwtAuthResponse jwtAuthResponse = new JwtAuthResponse();
        jwtAuthResponse.setToken(revokedToken.getJwtToken());
        jwtAuthResponse.setMessage(AppConstants.JWT_REVOKED);

        return new ResponseEntity<>(jwtAuthResponse, HttpStatus.OK);
    }

    @PreAuthorize("hasRole('ADMIN')")
    @PostMapping("/reset-password")
    public ResponseEntity<ApiResponse<?>> resetUserPassword(@RequestBody UserDto userDto) {
        UserDto updatedUser = userService.resetPassword(userDto.getUserId(), userDto.getPassword());
        //return new ResponseEntity<>(new ApiResponse("password has been reset", true), HttpStatus.OK);
        return ResponseEntity.ok(ResponseUtil.success(null, "password has been reset"));
    }

    @PostMapping("/send-otp")
    public ResponseEntity<ApiResponse<OtpDto>> loginOtp(@RequestBody OtpDto requestOtp) throws JsonProcessingException {
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
        otpLogService.saveOtp(otpLog);
        return ResponseEntity.ok(ResponseUtil.success(null, "otp sent successfully"));
    }

}
