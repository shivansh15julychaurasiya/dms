package ahc.dms.dao.entities;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import java.util.Date;

@Entity
@Table(name = "case_file_details")
@ToString
@Getter
@Setter
public class CaseFileDetails {

    @Id
    @SequenceGenerator(
            name = "cfd_seq_gen",            // Logical name for the generator
            sequenceName = "cfd_sequence",   // Actual DB sequence name
            allocationSize = 1               // Optional: defaults to 50; 1 = increment by 1
    )
    @GeneratedValue(
            strategy = GenerationType.SEQUENCE,
            generator = "cfd_seq_gen"
    )
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
