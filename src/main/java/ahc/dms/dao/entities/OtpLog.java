package ahc.dms.dao.entities;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.time.LocalDateTime;

@Entity
@Table(name = "otp_logs")
@Getter
@Setter
@NoArgsConstructor
public class OtpLog {
    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "otp_seq")
    @SequenceGenerator(name = "otp_seq", sequenceName = "otp_sequence", allocationSize = 1)
    @Column(name = "otp_id")
    private Long otpId;

    @Column(name = "login_id", nullable = false)
    private String loginId;

    private String otpValue;
    private String otpType;
    private LocalDateTime otpExpiry;
    private boolean otpStatus;

}