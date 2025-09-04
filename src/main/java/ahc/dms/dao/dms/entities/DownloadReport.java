package ahc.dms.dao.dms.entities;

import java.util.Date;
import java.util.List;

import org.hibernate.annotations.DynamicUpdate;

import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.OneToOne;
import jakarta.persistence.SequenceGenerator;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;
import jakarta.persistence.Transient;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
@Entity
@DynamicUpdate
@Table(name = "download_report")
@NoArgsConstructor       
@Data
@AllArgsConstructor         
@Builder                     
public class DownloadReport {

    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "download_report_seq")
    @SequenceGenerator(name = "download_report_seq", sequenceName = "download_report_seq", allocationSize = 1)
    @Column(name = "dr_id")
    private Long drId;

    @Column(name = "dr_amount")
    private Double drAmount;

    @Column(name = "dr_fd_mid")
    private Long drFdMid;

    @Column(name = "dr_cr_by")
    private Long drCrBy;

    @Column(name = "dr_cr_date")
    @Temporal(TemporalType.TIMESTAMP)
    private Date drCrDate;

    @Column(name = "dr_rec_status")
    private Integer drRecStatus;

    @Column(name = "dr_ip_address")
    private String drIpAddress;

    @Transient
    private List<DownloadFile> files;

    @OneToOne(cascade = CascadeType.PERSIST)
    @JoinColumn(name = "dr_fd_mid", insertable = false, updatable = false)
    private CaseFileDetails caseFileDetails;

    @OneToOne(cascade = CascadeType.PERSIST)
    @JoinColumn(name = "dr_cr_by", insertable = false, updatable = false)
    private User userDetail;

	
}
