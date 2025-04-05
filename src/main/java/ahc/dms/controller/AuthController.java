package ahc.dms.controller;

import ahc.dms.config.AppConstants;
import ahc.dms.payload.JwtAuthRequest;
import ahc.dms.payload.JwtAuthResponse;
import ahc.dms.payload.TokenDto;
import ahc.dms.payload.UserDto;
import ahc.dms.dao.services.TokenService;
import ahc.dms.dao.services.UserService;
import ahc.dms.exceptions.ApiException;
import ahc.dms.security.JwtTokenHelper;
import jakarta.servlet.http.HttpServletRequest;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/blog/auth")
public class AuthController {

    @Autowired
    private JwtTokenHelper jwtTokenHelper;
    @Autowired
    private UserDetailsService userDetailsService;
    @Autowired
    private TokenService tokenService;
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
        tokenDto.setTokenStatus(false);
        TokenDto revokedToken = tokenService.saveToken(tokenDto);

        JwtAuthResponse jwtAuthResponse = new JwtAuthResponse();
        jwtAuthResponse.setToken(token);
        jwtAuthResponse.setMessage(AppConstants.JWT_REVOKED);

        return new ResponseEntity<>(jwtAuthResponse, HttpStatus.OK);
    }


}
