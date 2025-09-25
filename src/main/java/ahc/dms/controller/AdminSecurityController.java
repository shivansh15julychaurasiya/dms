package ahc.dms.controller;

import java.util.Map;

import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import ahc.dms.dao.dms.services.LoginAttemptService;

@RestController
@RequestMapping("/dms/admin")
@PreAuthorize("hasRole('DMSAdmin')")
public class AdminSecurityController {

    private final LoginAttemptService loginAttemptService;

    public AdminSecurityController(LoginAttemptService loginAttemptService) {
        this.loginAttemptService = loginAttemptService;
    }

    @PostMapping("/unlock/{username}")
    public ResponseEntity<?> unlock(@PathVariable String username) {
    	
        loginAttemptService.unlock(username);
        return ResponseEntity.ok(Map.of("message", "User unlocked"));
    }
}

