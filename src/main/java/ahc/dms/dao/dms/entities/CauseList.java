package ahc.dms.dao.dms.entities;


import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Date;

import jakarta.persistence.*;
import lombok.Data;
import lombok.ToString;
import org.springframework.format.annotation.DateTimeFormat;

@Entity
@Table(name = "cause_list")
@Data
@ToString(exclude = {"cType", "caseType", "clType", "applicationStage", "courtMaster", "courtMasterForTransfer"})
public class CauseList {

    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "cause_list_seq")
    @SequenceGenerator(name = "cause_list_seq", sequenceName = "cause_list_seq", allocationSize = 1)
    @Column(name = "cl_id")
    private Long cl_id;

    @Column(name = "cl_case_type_mid")
    private Long cl_case_type_mid;

    @Column(name = "cl_case_no")
    private String cl_case_no;

    @Column(name = "cl_case_year")
    private Integer cl_case_year;

    @Column(name = "cl_court_no")
    private Integer cl_court_no;

    @Column(name = "cl_transfer_to")
    private Integer cl_transfer_to;

    @Column(name = "cl_serial_no")
    private Integer cl_serial_no;

    @Column(name = "cl_list_type_mid")
    private Long cl_list_type_mid;

    @Column(name = "cl_first_petitioner")
    private String cl_first_petitioner;

    @Column(name = "cl_first_respondent")
    private String cl_first_respondent;

    @Column(name = "cl_petitioner_council")
    private String cl_petitioner_council;

    @Column(name = "cl_respondent_council")
    private String cl_respondent_council;

    @Column(name = "cl_ano")
    private Integer cl_ano;

    @Column(name = "cl_ayr")
    private Integer cl_ayr;

    @Column(name = "cl_applawp")
    private String cl_applawp;

    @Column(name = "cl_applawr")
    private String cl_applawr;

    @Column(name = "cl_stage")
    private Integer cl_stage;

    @Column(name = "cl_dol")
    @DateTimeFormat(pattern = "yyyy-MM-dd")
    private LocalDate cl_dol;



    @Transient
    private Date cl_dol1;

    @Column(name = "cl_fd_mid")
    private Long cl_fd_mid;

    @Column(name = "cl_sequence")
    private Integer cl_sequence;

    @Column(name = "cl_rec_status")
    private Integer cl_rec_status;

    @Column(name = "cl_mod_by")
    private Long cl_mod_by;

    @Column(name = "cl_mod_date")
    private Date cl_mod_date;

    @Column(name = "cl_stage_id")
    private Long cl_stage_id;

    @Column(name = "cl_date")
    private Date cl_date;

    @Transient
    private boolean checked;

    @Transient
    private Integer count;

    @Transient
    private String clTypeData;

    @Transient
    private Integer cl_new_court_no;

    @Transient
    private Date listing_date;

    @Transient
    private boolean petAvailable = false;

    @Transient
    private CauseListType cType;

    @OneToOne(cascade = CascadeType.PERSIST)
    @JoinColumn(name = "cl_case_type_mid", insertable = false, updatable = false)
    private CaseType caseType;

    @OneToOne(cascade = CascadeType.PERSIST)
    @JoinColumn(name = "cl_list_type_mid", insertable = false, updatable = false)
    private CauseListType clType;

    @OneToOne(cascade = CascadeType.PERSIST)
    @JoinColumn(name = "cl_stage_id", insertable = false, updatable = false)
    private Lookup applicationStage;

    @OneToOne(cascade = CascadeType.PERSIST)
    @JoinColumn(name = "cl_court_no", insertable = false, updatable = false)
    private CourtMaster courtMaster;

    @OneToOne(cascade = CascadeType.PERSIST)
    @JoinColumn(name = "cl_transfer_to", insertable = false, updatable = false)
    private CourtMaster courtMasterForTransfer;
}
