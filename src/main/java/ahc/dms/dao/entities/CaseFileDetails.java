package ahc.dms.dao.entities;

import jakarta.persistence.*;
import lombok.Data;

import java.util.Date;

@Entity
@Table(name = "case_file_details")
@Data
public class CaseFileDetails {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "fd_id")
    private Long cfdId;

    @Column(name="case_type")
    private Long caseType;

    @Column(name="case_no")
    private String caseNo;

    @Column(name="case_year")
    private Integer caseYear;

    @Column(name="document_name")
    private String documentName;

    @Column(name="file_source")
    private String fileSource;

    @Column(name="rec_status")
    private int recStatus;

    @Column(name="stage_lid")
    private Long stageLid;

    @Column(name="cr_by")
    private Long crBy;

    @Column(name="cr_date")
    private Date crDate;

    @Column(name="mod_by")
    private Long modBy;

    @Column(name="mod_date")
    private Date modDate;

    @Column(name="disposal_date")
    private Date disposalDate;

    @Column(name="category_code")
    private Long categoryCode;

    @Column(name="district")
    private Long district;

    @Column(name="bench_type")
    private Long benchType;

    @Column(name="act_section")
    private String actSection;

    @Column(name="keywords")
    private String keywords;

    @Column(name="bench_code")
    private Long benchCode;

    @Column(name="assign_to")
    private Long assignTo;

    @Column(name="rc_flag")
    private Boolean rcFlag;


}
