package ahc.dms.dao.dms.entities;

import java.time.LocalDateTime;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import jakarta.persistence.Version;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Entity
@Table(name = "login_attempts")
public class LoginAttempt {

    @Id
    private String username;

    @Column(nullable = false)
    private int attempts;

    private LocalDateTime lastModified;

    private LocalDateTime lockedUntil;

    @Version
    private Long version;

   

    public LoginAttempt(String username) {
        this.username = username;
        this.attempts = 0;
    }

    // getters & setters ...
}
