package ahc.dms.dao.dms.entities;

import java.util.Date;

import jakarta.persistence.*;
import lombok.Data;

@Entity
@Table(name = "court_master")
@Data
public class CourtMaster {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "cm_id")
    private Integer cmId;

    @Column(name = "cm_name")
    private String cmName;

    @Column(name = "cm_value")
    private Integer cmValue;

    @Column(name = "cm_rec_status")
    private Integer cmRecStatus;

    @Column(name = "cm_bench_id",unique = false)
    private Integer cmBenchId ;

    @Column(name = "cm_cr_by")
    private Long cmCrBy;

    @Column(name = "cm_cr_date")
    private Date cmCrDate;

    @Column(name = "cm_mod_by")
    private Long cmModBy;

    @Column(name = "cm_mod_date")
    private Date cmModDate;

    @Transient
    private boolean updateFlag = false;
}
