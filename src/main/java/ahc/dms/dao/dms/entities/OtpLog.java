package ahc.dms.dao.dms.entities;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.UpdateTimestamp;
import org.springframework.data.annotation.CreatedBy;
import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import java.time.LocalDateTime;

@Entity
@Table(name = "otp_logs")
@Getter
@Setter
@NoArgsConstructor
@EntityListeners(AuditingEntityListener.class)
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

    // Audit Fields
    @CreationTimestamp
    @Column(name = "created_at", updatable = false)
    private LocalDateTime createdAt;
    @UpdateTimestamp
    @Column(name = "updated_at")
    private LocalDateTime updatedAt;
    @CreatedBy
    @Column(name = "created_by", updatable = false)
    private String createdBy;
    @LastModifiedBy
    @Column(name = "updated_by")
    private String updatedBy;

}