package ahc.dms.dao.dms.entities;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import org.hibernate.annotations.CreationTimestamp;
import org.springframework.data.annotation.CreatedBy;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import java.time.LocalDateTime;

@Entity
@Table(name = "request_logs")
@Getter
@Setter
@ToString
@NoArgsConstructor
@EntityListeners(AuditingEntityListener.class)
public class RequestLog {
	@Id
	@GeneratedValue(strategy=GenerationType.AUTO)
	@Column(name = "rlog_id")
	private Long rlogId;


    @Column(name = "ip_address")
    private String ipAddress;
    private String method;
    private String endpoint;

    // Audit Fields
    @CreationTimestamp
    @Column(name = "generated_at", updatable = false)
    private LocalDateTime generatedAt;
    @CreatedBy
    @Column(name = "generated_by", updatable = false)
    private String generatedBy;

}