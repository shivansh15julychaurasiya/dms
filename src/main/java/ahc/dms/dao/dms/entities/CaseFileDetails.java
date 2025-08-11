package ahc.dms.dao.dms.entities;

import jakarta.persistence.*;
import lombok.Data;
import org.hibernate.annotations.Where;

import java.time.LocalDateTime;
import java.util.List;

@Entity
@Table(name = "case_file_details")
@Data
public class CaseFileDetails {
    @Id
    @Column(name = "fd_id")
    private Long id;

    @ManyToOne
    @JoinColumn(name = "fd_case_type")
    private CaseType caseType;

    @Column(name = "fd_case_no")
    private String fdCaseNo;

    @Column(name = "fd_case_year")
    private Integer fdCaseYear;

    @Column(name = "fd_document_name")
    private String fdDocumentName;

    @Column(name = "fd_file_source")
    private String fdFileSource;

    @Column(name = "fd_rec_status")
    private Integer fdRecStatus;

    @Column(name = "fd_stage_lid")
    private Long fdStageLid;

    @Column(name = "fd_cr_by")
    private Long fdCrBy;

    @Column(name = "fd_cr_date")
    private LocalDateTime fdCrDate;

    @Column(name = "fd_mod_by")
    private Long fdModBy;

    @Column(name = "fd_mod_date")
    private LocalDateTime fdModDate;

    @Column(name = "fd_category_code")
    private Long fdCategoryCode;

    @Column(name = "fd_district")
    private Long fdDistrict;

    @Column(name = "fd_disposal_date")
    private LocalDateTime fdDisposalDate;

    @Column(name = "fd_act_section")
    private String fdActSection;

    @Column(name = "fd_keywords")
    private String fdKeywords;

    @Column(name = "fd_bench_type")
    private Long fdBenchType;

    @Column(name = "fd_bench_code")
    private Integer fdBenchCode;

    @Column(name = "fd_assign_to")
    private Long fdAssignTo;

    @Column(name = "fd_rc_flag")
    private Boolean fdRcFlag = false;

    @Column(name = "fd_first_petitioner")
    private String fdFirstPetitioner;

    @Column(name = "fd_first_respondent")
    private String fdFirstRespondent;

    @Column(name = "fd_rd_status")
    private String fdRdStatus;

    @Column(name = "act_section")
    private String actSection;

    @Column(name = "assign_to")
    private Long assignTo;

    @Column(name = "bench_code")
    private Long benchCode;

    @Column(name = "bench_type")
    private Long benchType;

    @Column(name = "case_no")
    private String caseNo;

    @Column(name = "case_type")
    private Long caseTypeId;

    @Column(name = "case_year")
    private Integer caseYear;

    @Column(name = "category_code")
    private Long categoryCode;

    @Column(name = "cr_by")
    private Long crBy;

    @Column(name = "created_at")
    private LocalDateTime createdAt;

    @Column(name = "created_by")
    private String createdBy;

    @Column(name = "disposal_date")
    private LocalDateTime disposalDate;

    @Column(name = "district")
    private Long district;

    @Column(name = "document_name")
    private String documentName;

    @Column(name = "file_source")
    private String fileSource;

    @Column(name = "keywords")
    private String keywords;

    @Column(name = "mod_by")
    private Long modBy;

    @Column(name = "rc_flag")
    private Boolean rcFlag;

    @Column(name = "rec_status")
    private Integer recStatus;

    @Column(name = "stage_lid")
    private Long stageLid;

    @Column(name = "updated_at")
    private LocalDateTime updatedAt;

    @Column(name = "updated_by")
    private String updatedBy;

    @Column(name = "version")
    private Long version;

    @OneToMany(cascade = CascadeType.ALL, fetch=FetchType.EAGER)
    @JoinColumn(name = "sd_fd_mid")
    @Where(clause="sd_rec_status=1")
    private List<SubDocument> subDocument;


    @OneToMany(cascade = CascadeType.ALL, fetch=FetchType.EAGER)
    @JoinColumn(name = "pt_fd_mid")
    @Where(clause="pt_rec_status=1")
    @OrderBy("pt_sequence ASC")
    private List<Petitioner> petitioners;

    @OneToMany(cascade = CascadeType.ALL, fetch=FetchType.EAGER)
    @JoinColumn(name = "rt_fd_mid")
    @Where(clause="rt_rec_status=1")
    @OrderBy("rt_sequence ASC")
    private List<Respondent> respondents;

    @OneToMany(cascade = CascadeType.ALL, fetch=FetchType.EAGER)
    @JoinColumn(name = "pc_fd_mid")
    @Where(clause="pc_rec_status=1")
    private List<PetitionerCounsel> pCounsels;

    @OneToMany(cascade = CascadeType.ALL, fetch=FetchType.EAGER)
    @JoinColumn(name = "rc_fd_mid")
    @Where(clause="rc_rec_status=1")
    private List<RespondentCounsel> rCounsels;



//    @OneToMany(mappedBy = "caseFile")
//    private List<SubApplication> subApplications;
}