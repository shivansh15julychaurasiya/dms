package ahc.dms.dao.dms.entities;

import jakarta.persistence.*;
import lombok.Data;

import java.time.LocalDateTime;

@Entity
@Table(name = "sub_applications")
@Data
public class SubApplication {
    @Id
    @Column(name = "sb_ap_id")
    private Long id;

    @ManyToOne
    @JoinColumn(name = "sb_ap_sd_mid")
    private SubDocument subDocument;

    @ManyToOne
    @JoinColumn(name = "sb_ap_fd_mid")
    private CaseFileDetails caseFile;

    @ManyToOne
    @JoinColumn(name = "sb_ap_at_mid")
    private ApplicationType applicationType;

    @Column(name = "sb_ap_no")
    private Integer applicationNo;

    @Column(name = "sb_ap_year")
    private Integer applicationYear;

    @Column(name = "sb_ap_rec_status")
    private Integer recStatus;

    @Column(name = "sb_ap_cr_date")
    private LocalDateTime createdDate;

    @Column(name = "sb_ap_from_page")
    private Long fromPage;

    @Column(name = "sb_ap_to_page")
    private Long toPage;

    @Column(name = "sb_ap_mod_by")
    private Long modifiedBy;

    @Column(name = "sb_ap_mod_date")
    private LocalDateTime modifiedDate;
}