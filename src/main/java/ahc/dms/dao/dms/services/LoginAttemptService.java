package ahc.dms.dao.dms.services;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import ahc.dms.dao.dms.entities.LoginAttempt;
import ahc.dms.dao.dms.repositories.LoginAttemptRepository;

import java.time.Duration;
import java.time.LocalDateTime;

@Service
public class LoginAttemptService {

    private final LoginAttemptRepository repo;

    // configurable values
    private final int MAX_ATTEMPTS = 5;
    private final Duration LOCK_DURATION = Duration.ofMinutes(15);

    public LoginAttemptService(LoginAttemptRepository repo) {
        this.repo = repo;
    }

    @Transactional
    public void loginFailed(String username) {
        LocalDateTime now = LocalDateTime.now();
        LoginAttempt attempt = repo.findById(username).orElseGet(() -> {
            LoginAttempt a = new LoginAttempt(username);
            a.setAttempts(0);
            return a;
        });

        // If previously locked but lock expired, reset
        if (attempt.getLockedUntil() != null && attempt.getLockedUntil().isBefore(now)) {
            attempt.setAttempts(0);
            attempt.setLockedUntil(null);
        }

        attempt.setAttempts(attempt.getAttempts() + 1);
        attempt.setLastModified(now);

        if (attempt.getAttempts() >= MAX_ATTEMPTS) {
            attempt.setLockedUntil(now.plus(LOCK_DURATION));
        }
        repo.save(attempt);
    }

    @Transactional
    public void loginSucceeded(String username) {
        // reset on success
        repo.findById(username).ifPresent(attempt -> {
            attempt.setAttempts(0);
            attempt.setLockedUntil(null);
            repo.delete(attempt); // or repo.save(attempt) to keep history
        });
    }

    @Transactional(readOnly = true)
    public boolean isBlocked(String username) {
        LocalDateTime now = LocalDateTime.now();
        return repo.findById(username).map(attempt -> {
            if (attempt.getLockedUntil() == null) return false;
            if (attempt.getLockedUntil().isBefore(now)) {
                // expired — treat as not blocked
                return false;
            }
            return true;
        }).orElse(false);
    }

    @Transactional
    public void unlock(String username) {
        repo.findById(username).ifPresent(attempt -> {
            attempt.setAttempts(0);
            attempt.setLockedUntil(null);
            repo.save(attempt);
        });
    }

    @Transactional(readOnly = true)
    public long remainingLockSeconds(String username) {
        LocalDateTime now = LocalDateTime.now();
        return repo.findById(username)
                .map(attempt -> {
                    if (attempt.getLockedUntil() == null) return 0L;
                    long secs = java.time.Duration.between(now, attempt.getLockedUntil()).getSeconds();
                    return Math.max(secs, 0L);
                })
                .orElse(0L);
    }
}
