package ahc.dms.dao.dms.entities;

import java.util.Date;

import jakarta.persistence.*;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import org.springframework.data.annotation.CreatedBy;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

@Entity
@Table(name = "respondent_counsel")
@EntityListeners(AuditingEntityListener.class)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class RespondentCounsel {

    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "rc_seq")
    @SequenceGenerator(name = "rc_seq", sequenceName = "rc_seq", allocationSize = 1)
    @Column(name = "rc_id")
    private Long rc_id;

    @Column(name = "rc_name")
    private String rc_name;

    @Column(name = "rc_rec_status")
    private Integer rc_rec_status;

    @Column(name = "rc_fd_mid")
    private Long rc_fd_mid;

    @Column(name = "rc_sequence")
    private Integer rc_sequence;

    @CreatedBy
    @Column(name = "rc_cr_by", updatable = false)
    private Long rc_cr_by;

    @CreatedDate
    @Column(name = "rc_cr_date", updatable = false)
    private Date rc_cr_date;

    @LastModifiedBy
    @Column(name = "rc_mod_by")
    private Long rc_mod_by;

    @LastModifiedDate
    @Column(name = "rc_mod_date")
    private Date rc_mod_date;
}
