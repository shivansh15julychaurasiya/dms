package ahc.dms.dao.dms.entities;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import org.hibernate.annotations.DialectOverride;

import java.util.Date;
import java.util.List;


@Entity
@Table(name = "court_master")
@Getter
@Setter
@ToString
public class CourtMaster {

@Id
@GeneratedValue(strategy = GenerationType.SEQUENCE,generator = "court_master_seq")
@SequenceGenerator(name = "court_master_seq",sequenceName = "court_master_seq",allocationSize = 1)

@Column(name = "cm_id")
private Integer cmId;

@Column(name = "cm_name")
private String cmName;

@Column(name = "cm_value")
private Integer cmValue;

@Column(name = "cm_rec_status")
private Integer cmRecStatus;

@Column(name = "cm_bench_id")
private Integer cmBenchId;

@Column(name = "cm_cr_by")
private Long cmCrBy;

@Column(name = "cm_cr_date")
private Date cmCrDate;

@Column(name = "cm_mod_by")
private Long cmModBy;

@Column(name = "cm_mod_date")
private Date cmModDate;

@Column(name = "cm_judges_name")
private String cmJudgesName;

//@OneToMany(cascade = CascadeType.ALL,fetch = FetchType.EAGER)
//@JoinColumn(name = "sb_cm_mid")
//@Where(clause="sb_rec_status = 1")
//private List<SubBenches> subBenches;

private transient boolean updateFlag = false;

}
